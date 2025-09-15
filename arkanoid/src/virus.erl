-module(virus).
-behaviour(gen_statem).

%% Client API (similar to ball.erl and bomb.erl)
-export([start_link/2, get_state/1, get_position/1, get_velocity/1, kill_virus/1, get_full_state_with_timing/1]).
-export([start_link/1, get_state/0, get_position/0, get_velocity/0]). %% Legacy support

%% gen_statem callbacks
-export([init/1, callback_mode/0, terminate/3, code_change/4]).
-export([active/3, dead/3]).

-define(SERVER, ?MODULE).
-define(DUPLICATION_INTERVAL, 7000). %% 7 seconds in milliseconds
-define(VIRUS_SPEED, 2). %% Virus movement speed

%%%===================================================================
%%% Client API
%%%===================================================================

%% @doc Starts the virus process with ID.
%% InitialData should be a map like #{pos => {X, Y}, vel => {VX, VY}, id => VirusId}
start_link(VirusId, InitialData) ->
    %% Create a unique atom name for registration (e.g., virus_1 -> virus_virus_1)
    ServerName = list_to_atom("virus_" ++ atom_to_list(VirusId)),
    gen_statem:start_link({local, ServerName}, ?MODULE, [InitialData#{id => VirusId}], []).

%% @doc Legacy start_link for backward compatibility
start_link(InitialData) ->
    VirusId = maps:get(id, InitialData, virus_1),
    start_link(VirusId, InitialData).

%% @doc Get the virus's current state atom by ID.
get_state(VirusId) ->
    ServerName = list_to_atom("virus_" ++ atom_to_list(VirusId)),
    gen_statem:call(ServerName, get_state).

%% @doc Get the virus's current position by ID.
get_position(VirusId) ->
    ServerName = list_to_atom("virus_" ++ atom_to_list(VirusId)),
    gen_statem:call(ServerName, get_position).

%% @doc Get the virus's current velocity by ID.
get_velocity(VirusId) ->
    ServerName = list_to_atom("virus_" ++ atom_to_list(VirusId)),
    gen_statem:call(ServerName, get_velocity).

%% @doc Kill the virus (when hit by ball)
kill_virus(VirusId) ->
    ServerName = list_to_atom("virus_" ++ atom_to_list(VirusId)),
    gen_statem:cast(ServerName, kill).

%% @doc Get full virus state including timing for handoffs
get_full_state_with_timing(VirusId) ->
    ServerName = list_to_atom("virus_" ++ atom_to_list(VirusId)),
    gen_statem:call(ServerName, get_full_state_with_timing).

%% Legacy API functions (for backward compatibility)
get_state() ->
    gen_statem:call(?SERVER, get_state).

get_position() ->
    gen_statem:call(?SERVER, get_position).

get_velocity() ->
    gen_statem:call(?SERVER, get_velocity).

%%%===================================================================
%%% gen_statem callbacks
%%%===================================================================

callback_mode() ->
    state_functions.

init([InitialData]) ->
    %% Virus starts in 'active' state
    _VirusId = maps:get(id, InitialData, virus_1),
    Position = maps:get(pos, InitialData, {400, 300}),
    Velocity = maps:get(vel, InitialData, {?VIRUS_SPEED, ?VIRUS_SPEED}),
    
    %% Start duplication timer - calculate from start time if provided
    DuplicationStartTime = maps:get(duplication_start_time, InitialData, erlang:system_time(millisecond)),
    CurrentTime = erlang:system_time(millisecond),
    ElapsedTime = CurrentTime - DuplicationStartTime,
    
    %% Calculate remaining time in current duplication cycle
    RemainingTime = ?DUPLICATION_INTERVAL - (ElapsedTime rem ?DUPLICATION_INTERVAL),
    ActualTime = max(1000, RemainingTime),  % Minimum 1 second to avoid too-fast duplications
    
    DuplicationTimer = erlang:send_after(ActualTime, self(), duplicate),
    
    StateData = InitialData#{
        duplication_timer => DuplicationTimer,
        duplication_start_time => DuplicationStartTime,
        pos => Position,
        vel => Velocity
    },
    
    {ok, active, StateData}.

terminate(_Reason, _State, Data) ->
    %% Cancel duplication timer if it exists
    case maps:get(duplication_timer, Data, undefined) of
        undefined -> ok;
        Timer -> erlang:cancel_timer(Timer)
    end,
    
    _VirusId = maps:get(id, Data, unknown),
    ok.

code_change(_Vsn, State, Data, _Extra) ->
    {ok, State, Data}.

%%%===================================================================
%%% State functions
%%%===================================================================

%% @private
%% @doc Handling events in the active state.
active({call, From}, get_state, Data) ->
    gen_statem:reply(From, {ok, active}),
    {keep_state, Data};

active({call, From}, get_position, Data) ->
    Position = maps:get(pos, Data, {0, 0}),
    gen_statem:reply(From, {ok, Position}),
    {keep_state, Data};

active({call, From}, get_velocity, Data) ->
    Velocity = maps:get(vel, Data, {0, 0}),
    gen_statem:reply(From, {ok, Velocity}),
    {keep_state, Data};

active({call, From}, get_full_state_with_timing, Data) ->
    %% Return position, velocity, and duplication start time for handoffs
    Position = maps:get(pos, Data, {0, 0}),
    Velocity = maps:get(vel, Data, {0, 0}),
    DuplicationStartTime = maps:get(duplication_start_time, Data, erlang:system_time(millisecond)),
    
    gen_statem:reply(From, {ok, {Position, Velocity, DuplicationStartTime}}),
    {keep_state, Data};

active(cast, kill, Data) ->
    %% Virus killed by ball collision
    _VirusId = maps:get(id, Data, unknown),
    
    %% Cancel duplication timer
    case maps:get(duplication_timer, Data, undefined) of
        undefined -> ok;
        Timer -> erlang:cancel_timer(Timer)
    end,
    
    %% Notify main node to remove from ETS and update counters
    MainNode = get_main_node(),
    gen_server:cast({game_server, MainNode}, {virus_killed, _VirusId}),
    
    {next_state, dead, Data#{duplication_timer => undefined}};

active(cast, {update_physics, NewPos, NewVel}, Data) ->
    %% Update position and velocity from physics engine
    VirusId = maps:get(id, Data, unknown),
    _OldPos = maps:get(pos, Data, {0, 0}),    %% Virus position updated
    
    NewData = Data#{pos => NewPos, vel => NewVel},
    
    %% Update ETS on main node
    MainNode = get_main_node(),
    gen_server:cast({game_server, MainNode}, 
                   {update_ets, {virus, VirusId}, NewPos, NewVel, get_quadrant_from_pos(NewPos)}),
    
    {keep_state, NewData};

active(info, duplicate, Data) ->
    %% Time to duplicate the virus
    VirusId = maps:get(id, Data, unknown),
    Position = maps:get(pos, Data, {400, 300}),
    
    %% Virus duplication triggered
    
    %% Request duplication from main node
    MainNode = get_main_node(),
    gen_server:cast({game_server, MainNode}, {duplicate_virus, VirusId, Position}),
    
    %% Schedule next duplication
    NewTimer = erlang:send_after(?DUPLICATION_INTERVAL, self(), duplicate),
    %% Next duplication scheduled
    NewData = Data#{duplication_timer => NewTimer},
    
    {keep_state, NewData};

active(cast, tick, Data) ->
    %% Physics tick - request physics calculation from collision manager
    VirusId = maps:get(id, Data, unknown),
    CurrentPos = maps:get(pos, Data, {400, 300}),
    CurrentVel = maps:get(vel, Data, {2, 2}),
    
    %% Get current quadrant for physics request
    CurrentQuadrant = get_quadrant_from_pos(CurrentPos),
    
    %% Request physics calculation from main node
    MainNode = get_main_node(),
    case gen_server:call({collision_manager, MainNode}, 
                        {calculate_physics, {virus, VirusId}, CurrentPos, CurrentVel, CurrentQuadrant}, 1000) of
        {NewPos, NewVel, NewQuadrant} ->
            %% Check if we need ownership transfer
            if CurrentQuadrant =/= NewQuadrant ->
                %% Request ownership transfer
                ObjectData = #{id => VirusId, pos => NewPos, vel => NewVel},
                game_server:request_ownership_transfer({virus, VirusId}, CurrentQuadrant, NewQuadrant, ObjectData);
            true ->
                %% Update our state and ETS
                gen_statem:cast(self(), {update_physics, NewPos, NewVel})
            end;
        _Error ->
            ok
    end,
    
    {keep_state, Data};

active(_EventType, _EventContent, Data) ->
    {keep_state, Data}.

%% @private
%% @doc Handling events in the dead state.
dead({call, From}, get_state, Data) ->
    gen_statem:reply(From, {ok, dead}),
    {keep_state, Data};

dead({call, From}, get_position, Data) ->
    Position = maps:get(pos, Data, {0, 0}),
    gen_statem:reply(From, {ok, Position}),
    {keep_state, Data};

dead({call, From}, get_velocity, Data) ->
    gen_statem:reply(From, {ok, {0, 0}}),
    {keep_state, Data};

dead(_EventType, _EventContent, Data) ->
    %% Dead virus ignores all other events
    {keep_state, Data}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% @private Determines which quadrant a coordinate belongs to.
get_quadrant_from_pos({X, Y}) when X < 400, Y < 300 -> q1;
get_quadrant_from_pos({X, Y}) when X >= 400, Y < 300 -> q2;
get_quadrant_from_pos({X, Y}) when X < 400, Y >= 300 -> q3;
get_quadrant_from_pos({X, Y}) when X >= 400, Y >= 300 -> q4.

%% @doc Get the main node from hardcoded config
get_main_node() ->
    %% Simple: use hardcoded config to find main node
    IPs = distributed_config:get_node_ips(),
    MainIP = maps:get(main, IPs),
    list_to_atom("main@" ++ MainIP).