-module(socket_server).
-behaviour(gen_server).

-record(state, {
    port :: integer(),
    listen_socket :: inet:socket(),
    client_socket :: inet:socket() | undefined,
    buffer = <<>> :: binary()
}).

-define(TCP_OPTIONS, [binary, {packet, raw}, {active, false}, {reuseaddr, true}]).

%% API
-export([start_link/1, send_update/1, format_object/1]). %% format_object exported for testing

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

start_link(Port) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [Port], []).

send_update(State) ->
    gen_server:cast(?MODULE, {send_update, State}).

%% gen_server Callbacks
init([Port]) ->
    case gen_tcp:listen(Port, ?TCP_OPTIONS) of
        {ok, ListenSocket} ->
            %% Start accepting connections
            self() ! accept_connection,
            {ok, #state{port = Port, listen_socket = ListenSocket}};
        {error, Reason} ->
            {stop, Reason}
    end.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast({send_update, GameState}, ServerState) ->
    case ServerState#state.client_socket of
        undefined ->
            %% No client connected
            {noreply, ServerState};
        Socket ->
            %% Convert Erlang game state record to JSON-compatible format
            FormattedPayload = format_game_state(GameState),
            
            JsonState = jsone:encode(#{
                type => <<"update_state">>,
                payload => FormattedPayload
            }),
            Length = byte_size(JsonState),
            LengthPrefix = <<Length:32/big>>,
            FullPacket = <<LengthPrefix/binary, JsonState/binary>>,
            
            case gen_tcp:send(Socket, FullPacket) of
                ok -> ok;
                {error, _SendError} -> ok
            end,
            {noreply, ServerState}
    end;
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({tcp, _Socket, Data}, State) ->
    %% Add new data to buffer
    NewBuffer = <<(State#state.buffer)/binary, Data/binary>>,
    
    %% Process complete packets from buffer
    {ProcessedBuffer, FinalState} = process_packets(NewBuffer, State),
    {noreply, FinalState#state{buffer = ProcessedBuffer}};
handle_info({tcp_closed, _Socket}, State) ->
    %% Client disconnected
    %% Schedule accepting new connections again
    self() ! accept_connection,
    {noreply, State#state{client_socket = undefined}};
handle_info({tcp_error, _Socket, _Reason}, State) ->
    %% Socket error occurred
    {noreply, State};
handle_info(accept_connection, State) ->
    case gen_tcp:accept(State#state.listen_socket) of
        {ok, ClientSocket} ->
            %% Configure the socket immediately
            inet:setopts(ClientSocket, [{active, true}, {nodelay, true}]),
            
            %% Send connection confirmation message
            TestMsg = jsone:encode(#{type => <<"connection_confirmed">>, message => <<"Hello from Erlang!">>}),
            Length = byte_size(TestMsg),
            LengthPrefix = <<Length:32/big>>,
            FullTestPacket = <<LengthPrefix/binary, TestMsg/binary>>,
            gen_tcp:send(ClientSocket, FullTestPacket),
            
            %% Close any existing client connection 
            case State#state.client_socket of
                undefined -> ok;
                OldSocket -> gen_tcp:close(OldSocket)
            end,
            
            {noreply, State#state{client_socket = ClientSocket}};
        {error, _Reason} ->
            %% Schedule retry
            erlang:send_after(1000, self(), accept_connection),
            {noreply, State}
    end;
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, State) ->
    case State#state.client_socket of
        undefined -> ok;
        Socket -> gen_tcp:close(Socket)
    end,
    case State#state.listen_socket of
        undefined -> ok;
        ListenSocket -> gen_tcp:close(ListenSocket)
    end,
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% Internal functions
process_packets(Buffer, State) ->
    case Buffer of
        <<Length:32/big, Rest/binary>> when byte_size(Rest) >= Length ->
            %% We have a complete packet
            <<JsonData:Length/binary, Remaining/binary>> = Rest,
            
            %% Parse and handle the JSON message
            try
                Message = jsone:decode(JsonData),
                NewState = handle_message(Message, State),
                %% Continue processing remaining buffer
                process_packets(Remaining, NewState)
            catch
                Error:Reason ->
                    io:format("Error parsing JSON: ~p:~p~n", [Error, Reason]),
                    %% Skip this packet and continue
                    process_packets(Remaining, State)
            end;
        _ ->
            %% Incomplete packet, keep buffer as-is
            {Buffer, State}
    end.

handle_message(#{<<"type">> := <<"game_command">>, <<"command">> := <<"start_game">>}, State) ->
    game_server:start_game(),
    State;
handle_message(#{<<"type">> := <<"key_event">>, <<"key">> := Key}, State) ->
    handle_key_event(Key),
    State;
handle_message(Message, State) ->
    io:format("Unknown message received: ~p~n", [Message]),
    State.

%% Handle key events for paddle movement and ball launch
handle_key_event(<<"a">>) ->
    game_server:move_paddle(left);
handle_key_event(<<"d">>) ->
    game_server:move_paddle(right);
handle_key_event(<<"space">>) ->
    game_server:launch_ball();
handle_key_event(<<"b">>) ->
    game_server:spawn_new_ball();
handle_key_event(<<"v">>) ->
    game_server:test_spawn_virus();
handle_key_event(_Key) ->
    ok.

%% Convert Erlang game state record to JSON-compatible map
format_game_state(GameState) ->
    %% Extract game objects from ETS table
    Objects = case ets:tab2list(game_objects) of
        undefined -> [];
        ObjectList -> [format_object(Obj) || Obj <- ObjectList]
    end,
    
    %% Extract the fields we need for JSON
    case GameState of
        {state, Score, Lives, Level, _TimerRef, _BallCounter, _BrickCounter, _BombCounter, _VirusCounter, _VirusLimit, _Respawning} ->
            %% Latest format with all counters including bomb_counter
            #{
                score => Score,
                lives => Lives, 
                level => Level,
                objects => Objects
            };
        {state, Score, Lives, Level, _TimerRef, _BallCounter, _Respawning} ->
            %% Format with ball_counter and respawning fields
            #{
                score => Score,
                lives => Lives, 
                level => Level,
                objects => Objects
            };
        {state, Score, Lives, Level, _TimerRef, _BallCounter} ->
            %% Format with ball_counter but no respawning field
            #{
                score => Score,
                lives => Lives, 
                level => Level,
                objects => Objects
            };
        {state, Score, Lives, Level, _TimerRef} ->
            %% Original format
            #{
                score => Score,
                lives => Lives, 
                level => Level,
                objects => Objects
            };
        _ ->
            %% Fallback for unexpected format
            #{
                score => 0,
                lives => 3,
                level => 1,
                objects => Objects
            }
    end.

%% Convert ETS object tuple to JSON-compatible map  
format_object({{ball, BallId}, Position, _Velocity, Owner}) ->
    %% Handle ball with ID
    #{
        type => <<"ball">>,
        id => atom_to_binary(BallId, utf8),
        pos => format_position(Position),
        owner => atom_to_binary(Owner, utf8)
    };
format_object({{brick, BrickId}, Position, Health, Owner, SpecialEffect}) ->
    %% Handle brick with ID, health, and special effect
    #{
        type => <<"brick">>,
        id => atom_to_binary(BrickId, utf8),
        pos => format_position(Position),
        health => Health,
        special_effect => atom_to_binary(SpecialEffect, utf8),
        owner => atom_to_binary(Owner, utf8)
    };
format_object({{virus, VirusId}, Position, _Velocity, Owner, _DuplicationStartTime}) ->
    %% Handle virus with ID and duplication timing (5th element ignored for GUI)
    #{
        type => <<"virus">>,
        id => atom_to_binary(VirusId, utf8),
        pos => format_position(Position),
        owner => atom_to_binary(Owner, utf8)
    };
format_object({{virus, VirusId}, Position, _Velocity, Owner}) ->
    %% Handle virus with ID (old 4-element format - backward compatibility)
    #{
        type => <<"virus">>,
        id => atom_to_binary(VirusId, utf8),
        pos => format_position(Position),
        owner => atom_to_binary(Owner, utf8)
    };
format_object({{bomb, BombId}, Position, _Velocity, Owner}) ->
    %% Handle bomb with ID
    #{
        type => <<"bomb">>,
        id => atom_to_binary(BombId, utf8),
        pos => format_position(Position),
        owner => atom_to_binary(Owner, utf8)
    };
format_object({ObjectType, Position, _Velocity, Owner}) ->
    %% Handle other objects (paddle, brick, etc.)
    #{
        type => atom_to_binary(ObjectType, utf8),
        pos => format_position(Position),
        owner => atom_to_binary(Owner, utf8)
    };
format_object({{ball, BallId}, Position, _Velocity}) ->
    %% Handle ball with ID without owner
    #{
        type => <<"ball">>,
        id => atom_to_binary(BallId, utf8),
        pos => format_position(Position)
    };
format_object({ObjectType, Position, _Velocity}) ->
    %% Handle objects without owner
    #{
        type => atom_to_binary(ObjectType, utf8),
        pos => format_position(Position)
    }.

%% Convert position tuple to map
format_position({X, Y}) ->
    #{x => X, y => Y}.
