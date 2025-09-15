-module(fault_recovery_manager).
-behaviour(gen_server).

%% API
-export([start_link/0]).
-export([handle_quadrant_failure/3, is_quadrant_failed/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {
    failed_quadrants = #{},  
    local_fallbacks = #{}    
}).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% @doc Handle a quadrant failure notification
handle_quadrant_failure(Quadrant, FailedNode, Reason) ->
    gen_server:cast(?MODULE, {quadrant_failure, Quadrant, FailedNode, Reason}).

%% @doc Check if a quadrant is currently in failed state
is_quadrant_failed(Quadrant) ->
    try
        gen_server:call(?MODULE, {is_quadrant_failed, Quadrant}, 1000)
    catch
        exit:{timeout, _} -> false;  %% If manager not available, assume healthy
        _:_ -> false
    end.

%%====================================================================
%% gen_server callbacks
%%====================================================================

init([]) ->
    %% Monitor node connections for reconnection detection
    net_kernel:monitor_nodes(true),
    io:format("Fault recovery manager started - monitoring quadrant health~n"),
    {ok, #state{}}.

handle_call({is_quadrant_failed, Quadrant}, _From, State) ->
    IsFailed = maps:is_key(Quadrant, State#state.failed_quadrants),
    {reply, IsFailed, State};

handle_call({get_routing_for_quadrant, Quadrant}, _From, State) ->
    %% Routing decision for slave nodes
    case maps:is_key(Quadrant, State#state.failed_quadrants) of
        true ->
            %% Failed quadrant - route to main node
            MainNode = node(),
            io:format("Quadrant ~p failed - routing to main node ~p~n", [Quadrant, MainNode]),
            {reply, {ok, MainNode}, State};
        false ->
            %% Healthy quadrant - route to original slave node
            SlaveNode = get_default_slave_node(Quadrant),
            io:format("Quadrant ~p healthy - routing to slave ~p~n", [Quadrant, SlaveNode]),
            {reply, {ok, SlaveNode}, State}
    end;

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast({quadrant_failure, Quadrant, FailedNode, Reason}, State) ->
    io:format("Fault recovery manager received failure notification~n"),
    io:format("Fault detected: Quadrant ~p on ~p failed: ~p~n", [Quadrant, FailedNode, Reason]),
    io:format("Checking if should create local fallback~n"),
    
    case should_create_local_fallback(Reason, FailedNode) of
        true ->
            %% Node-level failure - create local fallback
            io:format("Node-level failure detected - creating local fallback for ~p~n", [Quadrant]),
            FailedQuadrants = State#state.failed_quadrants,
            FailureInfo = #{
                node => FailedNode, 
                reason => Reason, 
                timestamp => erlang:system_time(millisecond)
            },
            NewFailedQuadrants = maps:put(Quadrant, FailureInfo, FailedQuadrants),
            
            %% Start local fallback and recover objects
            io:format("Starting local fallback supervisor for quadrant ~p~n", [Quadrant]),
            start_local_fallback(Quadrant),
            io:format("Recovering objects from ETS for quadrant ~p~n", [Quadrant]),
            recover_quadrant_objects(Quadrant),
            io:format("Fallback setup completed for quadrant ~p~n", [Quadrant]),
            
            {noreply, State#state{failed_quadrants = NewFailedQuadrants}};
        false ->
            %% Process-level failure - let slave_sup handle restart
            io:format("Process failure - letting slave_sup restart ~p on ~p~n", [Quadrant, FailedNode]),
            
            %% Wait a bit for slave_sup to restart, then check if recovery worked
            timer:send_after(5000, self(), {check_recovery, Quadrant, FailedNode, Reason}),
            
            {noreply, State}
    end;

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({nodedown, Node}, State) ->
    %% Additional node failure detection via net_kernel
    io:format("Node ~p down detected via net_kernel~n", [Node]),
    case node_to_quadrant(Node) of
        unknown ->
            io:format("Node ~p is not a slave node, ignoring~n", [Node]);
        Quadrant ->
            io:format("Node ~p affects quadrant ~p, checking if already handled~n", [Node, Quadrant]),
            case maps:is_key(Quadrant, State#state.failed_quadrants) of
                true ->
                    io:format("Quadrant ~p already in fallback mode~n", [Quadrant]);
                false ->
                    io:format("Creating fallback for unhandled quadrant ~p failure~n", [Quadrant]),
                    %% This is a backup detection - create fallback
                    self() ! {quadrant_failure, Quadrant, Node, nodedown}
            end
    end,
    {noreply, State};

handle_info({check_recovery, Quadrant, FailedNode, Reason}, State) ->
    %% Check if slave_sup successfully restarted the quadrant manager
    io:format("Checking if slave_sup restarted ~p on ~p~n", [Quadrant, FailedNode]),
    
    case is_node_responsive(FailedNode, Quadrant) of
        true ->
            io:format("Quadrant ~p on ~p restarted by slave_sup~n", [Quadrant, FailedNode]),
            {noreply, State};
        false ->
            io:format("Failed recovery - creating local fallback for ~p~n", [Quadrant]),
            %% slave_sup couldn't restart - treat as node failure now
            FailedQuadrants = State#state.failed_quadrants,
            FailureInfo = #{
                node => FailedNode, 
                reason => Reason, 
                timestamp => erlang:system_time(millisecond)
            },
            NewFailedQuadrants = maps:put(Quadrant, FailureInfo, FailedQuadrants),
            
            %% Start local fallback and recover objects (delayed recovery)
            start_local_fallback(Quadrant),
            recover_quadrant_objects(Quadrant),
            
            {noreply, State#state{failed_quadrants = NewFailedQuadrants}}
    end;

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal functions
%%====================================================================

%% @doc Determine if we should create local fallback based on failure reason
should_create_local_fallback(Reason, _FailedNode) ->
    case Reason of
        noconnection -> true;           
        nodedown -> true;               
        {slave_sup_died, _} -> true;    
        killed -> false;                
        normal -> false;                
        shutdown -> false;              
        {shutdown, _} -> false;         
        _ -> false                      
    end.

%% @doc Check if a node and quadrant are responsive
is_node_responsive(Node, Quadrant) ->
    try
        %% Try to ping the quadrant manager on the node
        QuadrantManager = list_to_atom("quadrant_manager_" ++ atom_to_list(Quadrant)),
        case rpc:call(Node, erlang, whereis, [QuadrantManager], 2000) of
            {badrpc, _} -> false;  %% Node or RPC failed
            undefined -> false;    %% Process not registered
            Pid when is_pid(Pid) -> 
                %% Process exists, check if it's alive
                rpc:call(Node, erlang, is_process_alive, [Pid], 1000) =:= true;
            _ -> false
        end
    catch
        _:_ -> false
    end.

%% @doc Start local fallback quadrant manager using fallback_sup
start_local_fallback(Quadrant) ->
    io:format("Creating local fallback for failed quadrant ~p~n", [Quadrant]),
    %% Use fallback_sup.erl for local fallback
    FallbackId = list_to_atom("fallback_" ++ atom_to_list(Quadrant)),
    io:format("Calling fallback_sup:start_link(~p) under arkanoid_sup~n", [Quadrant]),
    FallbackSpec = #{
        id => FallbackId,
        start => {fallback_sup, start_link, [Quadrant]},
        restart => temporary,  %% Remove when slave recovers
        shutdown => 5000,
        type => supervisor,
        modules => [fallback_sup]
    },
    io:format("Adding ~p child to arkanoid_sup~n", [FallbackId]),
    case supervisor:start_child(arkanoid_sup, FallbackSpec) of
        {ok, _Pid} ->
            io:format("Local fallback started ~p for quadrant ~p~n", [FallbackId, Quadrant]),
            ok;
        {error, {already_started, _Pid}} ->
            io:format("Local fallback ~p already running for quadrant ~p~n", [FallbackId, Quadrant]),
            ok;
        Error ->
            io:format("Fallback failed - could not start ~p for quadrant ~p: ~p~n", [FallbackId, Quadrant, Error]),
            Error
    end.

%% @doc Recover all objects from ETS that belonged to the failed quadrant
recover_quadrant_objects(Quadrant) ->
    try
        %% Get all objects that were owned by the failed quadrant
        AllObjects = ets:tab2list(game_objects),
        QuadrantObjects = lists:filter(fun(Entry) ->
            case Entry of
                {_Key, _Pos, _Vel, Owner} when Owner =:= Quadrant -> true;
                {_Key, _Pos, _Vel, Owner, _Extra} when Owner =:= Quadrant -> true;
                _ -> false
            end
        end, AllObjects),
        
        io:format("Found ~p objects to recover for quadrant ~p~n", [length(QuadrantObjects), Quadrant]),
        
        %% Respawn each object locally
        lists:foreach(fun(Object) -> 
            respawn_object_locally(Object, Quadrant) 
        end, QuadrantObjects),
        
        io:format("Object recovery completed for quadrant ~p~n", [Quadrant]),
        ok
    catch
        Error:Reason ->
            io:format("Recovery failed - error recovering objects for ~p: ~p:~p~n", [Quadrant, Error, Reason]),
            {error, {Error, Reason}}
    end.

%% @doc Respawn individual objects on local fallback
respawn_object_locally({{ball, BallId}, Position, Velocity, _Owner}, Quadrant) ->
    %% Update ETS ownership to local fallback
    gen_server:cast(game_server, {update_ets, {ball, BallId}, Position, Velocity, Quadrant}),
    
    %% Spawn ball process on local fallback quadrant manager
    LocalQuadrantManager = list_to_atom("quadrant_manager_" ++ atom_to_list(Quadrant)),
    gen_server:cast(LocalQuadrantManager, {spawn_ball, #{id => BallId, pos => Position, vel => Velocity}}),
    
    io:format("Recovered ball ~p at ~p on local fallback ~p~n", [BallId, Position, Quadrant]);

respawn_object_locally({{virus, VirusId}, Position, Velocity, _Owner, DuplicationStartTime}, Quadrant) ->
    %% Update ETS with preserved timing
    gen_server:cast(game_server, {update_ets_virus, VirusId, Position, Velocity, Quadrant, DuplicationStartTime}),
    
    %% Spawn virus with preserved duplication timing
    LocalQuadrantManager = list_to_atom("quadrant_manager_" ++ atom_to_list(Quadrant)),
    InitialData = #{id => VirusId, pos => Position, vel => Velocity, duplication_start_time => DuplicationStartTime},
    gen_server:cast(LocalQuadrantManager, {spawn_virus, InitialData}),
    
    io:format("Recovered virus ~p at ~p on local fallback ~p~n", [VirusId, Position, Quadrant]);

respawn_object_locally({{bomb, BombId}, Position, Velocity, _Owner}, Quadrant) ->
    %% Update ETS ownership to local fallback
    gen_server:cast(game_server, {update_ets, {bomb, BombId}, Position, Velocity, Quadrant}),
    
    %% Spawn bomb process on local fallback
    LocalQuadrantManager = list_to_atom("quadrant_manager_" ++ atom_to_list(Quadrant)),
    gen_server:cast(LocalQuadrantManager, {spawn_bomb, #{id => BombId, pos => Position, vel => Velocity}}),
    
    io:format("Recovered bomb ~p at ~p on local fallback ~p~n", [BombId, Position, Quadrant]);

respawn_object_locally({paddle, Position, Velocity, _Owner}, Quadrant) ->
    %% Update ETS ownership to local fallback
    gen_server:cast(game_server, {update_ets, paddle, Position, Velocity, Quadrant}),
    
    %% Spawn paddle process on local fallback quadrant manager
    LocalQuadrantManager = list_to_atom("quadrant_manager_" ++ atom_to_list(Quadrant)),
    gen_server:cast(LocalQuadrantManager, {take_ownership, paddle}),
    
    io:format("Recovered paddle at ~p on local fallback ~p (ownership transferred)~n", [Position, Quadrant]);

respawn_object_locally({{brick, BrickId}, Position, Health, _Owner, SpecialEffect}, Quadrant) ->
    %% Update ETS ownership to local fallback (preserve full brick structure)
    gen_server:cast(game_server, {update_ets_brick, BrickId, Position, Health, Quadrant, SpecialEffect}),
    
    %% Spawn brick process on local fallback
    LocalQuadrantManager = list_to_atom("quadrant_manager_" ++ atom_to_list(Quadrant)),
    gen_server:cast(LocalQuadrantManager, {spawn_brick, #{id => BrickId, pos => Position, health => Health, special_effect => SpecialEffect}}),
    
    io:format("Recovered brick ~p at ~p (health=~p, effect=~p) on local fallback ~p~n", [BrickId, Position, Health, SpecialEffect, Quadrant]);

respawn_object_locally(Object, Quadrant) ->
    %% Unknown object type - log and skip
    io:format("Unknown object type for ~p, skipping: ~p~n", [Quadrant, Object]).

%% @doc Helper function to determine quadrant from node name (for nodedown events)
node_to_quadrant(Node) when is_atom(Node) ->
    NodeStr = atom_to_list(Node),
    case NodeStr of
        "slave1@" ++ _ -> q1;
        "slave2@" ++ _ -> q2;
        "slave3@" ++ _ -> q3;
        "slave4@" ++ _ -> q4;
        _ -> unknown
    end.

%% @doc Get default slave node for quadrant (used by routing service)
get_default_slave_node(Quadrant) ->
    distributed_config:get_node_from_quadrant(Quadrant).

