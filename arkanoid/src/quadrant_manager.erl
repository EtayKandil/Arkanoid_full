-module(quadrant_manager).
-behaviour(gen_server).

%% API
-export([start_link/1, start_link_remote/2]).
-export([take_ownership/2, move_paddle/2, stop_paddle/2, get_node_from_quadrant/1]).
-export([update_object_position/4, release_ownership/4, test_distributed_message/1, crash/1]).
-export([spawn_ball/2, stop_owned_ball/1, launch_owned_ball/1]). %% Ball API for handoff
-export([spawn_ball_idle/2]). %% For respawning ball in idle state
-export([stop_owned_ball_by_id/2]). %% Stop specific ball by ID
-export([spawn_brick/2, stop_owned_brick_by_id/2]). %% Brick process management
-export([damage_brick/2]). %% Brick damage system
-export([spawn_bomb/2, stop_owned_bomb_by_id/2]). %% Bomb process management
-export([spawn_virus/2, stop_owned_virus_by_id/2]). %% Virus process management

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(SERVER(Quadrant), list_to_atom("quadrant_manager_" ++ atom_to_list(Quadrant))).
-define(GAME_LOOP_INTERVAL, 16). %% approx 60 FPS for the gamers among us
-define(DUPLICATION_INTERVAL, 7000). %% 7 seconds in milliseconds (for virus handoff fallback)

-record(state, {
    quadrant :: atom(),
    boundaries :: map(),
    owned_balls = [] :: [atom()],  %% List of ball IDs owned by this quadrant
    owned_bricks = [] :: [atom()], %% List of brick IDs owned by this quadrant
    owned_bombs = [] :: [atom()],  %% List of bomb IDs owned by this quadrant
    owned_viruses = [] :: [atom()], %% List of virus IDs owned by this quadrant
    owns_paddle = false :: boolean(),
    ball_tick_timers = #{} :: #{atom() => reference()},  %% Map of BallId -> Timer
    bomb_tick_timers = #{} :: #{atom() => reference()},  %% Map of BombId -> Timer
    virus_tick_timers = #{} :: #{atom() => reference()}  %% Map of VirusId -> Timer
}).

%%%===================================================================
%%% API
%%%===================================================================

start_link(Quadrant) ->
    gen_server:start_link({local, ?SERVER(Quadrant)}, ?MODULE, [Quadrant], []).

start_link_remote(Quadrant, RemoteNode) ->
    io:format("Called for ~p on ~p (FROM MAIN NODE)~n", [Quadrant, RemoteNode]),
    io:format("This is the KEY connection function~n"),
    
    %% Enhanced debug: Check node reachability first
    case net_adm:ping(RemoteNode) of
        pong ->
            io:format("Node ~p is REACHABLE~n", [RemoteNode]);
        pang ->
            io:format("Node ~p is UNREACHABLE - RPC will fail~n", [RemoteNode])
    end,
    
    %% Show current node connectivity
    io:format("Currently connected nodes: ~p~n", [nodes()]),
    
    %%  Start quadrant manager via slave_sup 
    io:format("Making RPC call to ~p slave_sup:start_quadrant_manager(~p)~n", [RemoteNode, Quadrant]),
    case rpc:call(RemoteNode, slave_sup, start_quadrant_manager, [Quadrant], 10000) of
        {ok, _QuadrantPid} ->
            io:format("RPC SUCCESS! Quadrant ~p started on ~p~n", [Quadrant, RemoteNode]),
            
            %%  Find slave_sup PID to monitor (not the quadrant manager)
            io:format("Looking for slave_sup PID on ~p~n", [RemoteNode]),
            case rpc:call(RemoteNode, erlang, whereis, [slave_sup], 5000) of
                SlavePid when is_pid(SlavePid) ->
                    io:format("Found slave_sup ~p on ~p~n", [SlavePid, RemoteNode]),
                    
                    %%  Create proxy that monitors slave_sup
                    io:format("Creating proxy to monitor slave_sup (NOT quadrant manager)~n"),
                    ProxyPid = spawn_link(fun() -> 
                        _MonitorRef = monitor(process, SlavePid),
                        io:format("Started monitoring slave_sup ~p for quadrant ~p~n", [SlavePid, Quadrant]),
                        slave_sup_proxy_loop(SlavePid, Quadrant, RemoteNode)
                    end),
                    io:format("Proxy ~p created for ~p~n", [ProxyPid, Quadrant]),
                    {ok, ProxyPid};
                Error ->
                    io:format("Failed to find slave_sup on ~p: ~p~n", [RemoteNode, Error]),
                    Error
            end;
        {error, {already_started, _Pid}} ->
            %% Quadrant manager already exists, just monitor slave_sup
            io:format("Quadrant ~p already started on ~p, monitoring slave_sup~n", [Quadrant, RemoteNode]),
            case rpc:call(RemoteNode, erlang, whereis, [slave_sup], 5000) of
                SlavePid when is_pid(SlavePid) ->
                    ProxyPid = spawn_link(fun() -> 
                        _MonitorRef = monitor(process, SlavePid),
                        slave_sup_proxy_loop(SlavePid, Quadrant, RemoteNode)
                    end),
                    {ok, ProxyPid};
                Error ->
                    io:format("Failed to find slave_sup on ~p: ~p~n", [RemoteNode, Error]),
                    Error
            end;
        Error ->
            io:format("Failed to start ~p via slave_sup on ~p: ~p~n", [Quadrant, RemoteNode, Error]),
            Error
    end.

%% @doc Tell a quadrant to take ownership of an object (ball or paddle).
take_ownership(TargetQuadrant, Object) ->
    TargetNode = get_node_from_quadrant(TargetQuadrant),
    TargetProcess = {?SERVER(TargetQuadrant), TargetNode},
    gen_server:cast(TargetProcess, {take_ownership, Object}).

%% @doc Tell a quadrant to stop its owned ball process (e.g., when ball is lost).
stop_owned_ball(TargetQuadrant) ->
    TargetNode = get_node_from_quadrant(TargetQuadrant),
    TargetProcess = {?SERVER(TargetQuadrant), TargetNode},
    gen_server:cast(TargetProcess, stop_ball).

%% @doc Tell a quadrant to stop a specific ball by ID.
stop_owned_ball_by_id(TargetQuadrant, BallId) ->
    TargetNode = get_node_from_quadrant(TargetQuadrant),
    TargetProcess = {?SERVER(TargetQuadrant), TargetNode},
    gen_server:cast(TargetProcess, {stop_ball_by_id, BallId}).

%% @doc Tell a quadrant to launch its owned ball.
launch_owned_ball(TargetQuadrant) ->
    TargetNode = get_node_from_quadrant(TargetQuadrant),
    TargetProcess = {?SERVER(TargetQuadrant), TargetNode},
    gen_server:cast(TargetProcess, launch_ball).

%% @doc Remotely spawn a ball on this quadrant's node
spawn_ball(TargetQuadrant, InitialData) ->
    TargetNode = get_node_from_quadrant(TargetQuadrant),
    TargetProcess = {?SERVER(TargetQuadrant), TargetNode},
    gen_server:cast(TargetProcess, {spawn_ball, InitialData}).

%% @doc Remotely spawn a ball in idle state (for respawn)
spawn_ball_idle(TargetQuadrant, InitialData) ->
    TargetNode = get_node_from_quadrant(TargetQuadrant),
    TargetProcess = {?SERVER(TargetQuadrant), TargetNode},
    gen_server:cast(TargetProcess, {spawn_ball_idle, InitialData}).

%% @doc Send a paddle movement command to a quadrant.
move_paddle(TargetQuadrant, Direction) ->
    TargetNode = get_node_from_quadrant(TargetQuadrant),
    TargetProcess = {?SERVER(TargetQuadrant), TargetNode},
    gen_server:cast(TargetProcess, {move_paddle, Direction}).

%% @doc Send a paddle stop command to a quadrant.
stop_paddle(TargetQuadrant, Direction) ->
    TargetNode = get_node_from_quadrant(TargetQuadrant),
    TargetProcess = {?SERVER(TargetQuadrant), TargetNode},
    gen_server:cast(TargetProcess, {stop_paddle, Direction}).

%% @doc Get the node name for a given quadrant - with fault tolerance support.
get_node_from_quadrant(Quadrant) -> 
    %% Check if this is the main node or a slave node
    CurrentNode = node(),
    case atom_to_list(CurrentNode) of
        "main@" ++ _ ->
            %% Main node - check if quadrant is in fallback mode
            case fault_recovery_manager:is_quadrant_failed(Quadrant) of
                true ->
                    %% Quadrant failed - return main node for local fallback
                    MainNode = get_main_node(),
                    io:format("Quadrant ~p is FAILED - redirecting to main node ~p~n", [Quadrant, MainNode]),
                    MainNode;
                false ->
                    %% Quadrant healthy - return original slave node
                    SlaveNode = distributed_config:get_node_from_quadrant(Quadrant),
                    io:format("Quadrant ~p -> slave node ~p~n", [Quadrant, SlaveNode]),
                    SlaveNode
            end;
        _ ->
            %% Slave nodes - ask main node for routing decision
            MainNode = get_main_node(),
            %% Slave asking main node for routing
            case gen_server:call({fault_recovery_manager, MainNode}, {get_routing_for_quadrant, Quadrant}, 5000) of
                {ok, TargetNode} ->
                    %% Routing decision received from main node
                    TargetNode;
                Error ->
                    io:format("Failed to get routing from main node: ~p, using fallback~n", [Error]),
                    get_default_node_for_quadrant(Quadrant)
            end
    end.

%% @doc Get default slave node for quadrant (used by slave nodes)
get_default_node_for_quadrant(Quadrant) ->
    distributed_config:get_node_from_quadrant(Quadrant).

%% Additional API functions
update_object_position(Quadrant, Object, Position, Velocity) ->
    TargetNode = get_node_from_quadrant(Quadrant),
    gen_server:cast({?SERVER(Quadrant), TargetNode}, {update_position, Object, Position, Velocity}).

release_ownership(Quadrant, Object, Position, Velocity) ->
    TargetNode = get_node_from_quadrant(Quadrant),
    gen_server:cast({?SERVER(Quadrant), TargetNode}, {release_ownership, Object, Position, Velocity}).

test_distributed_message(Quadrant) ->
    TargetNode = get_node_from_quadrant(Quadrant),
    gen_server:call({?SERVER(Quadrant), TargetNode}, test_message, 5000).

crash(Quadrant) ->
    TargetNode = get_node_from_quadrant(Quadrant),
    gen_server:cast({?SERVER(Quadrant), TargetNode}, crash).

get_main_node() ->
    %%  use hardcoded config to find main node
    IPs = distributed_config:get_node_ips(),
    MainIP = maps:get(main, IPs),
    list_to_atom("main@" ++ MainIP).

%% @doc Get the appropriate supervisor for this quadrant manager
%% Returns slave_sup for slave nodes, fallback_sup_qX for local fallback
get_local_supervisor(Quadrant) ->
    %% Check if we're running as a fallback by looking for the fallback supervisor
    FallbackSupName = list_to_atom("fallback_sup_" ++ atom_to_list(Quadrant)),
    case whereis(FallbackSupName) of
        undefined ->
            %% No fallback supervisor exists - we're on a slave node
            slave_sup;
        _Pid ->
            %% Fallback supervisor exists - we're running as fallback on main node
            FallbackSupName
    end.

%% @doc Unified object spawning that works for both slave nodes and fallback
%% Handles the supervisor interface differences automatically
spawn_object_process(ObjectType, ObjectId, InitialData, Quadrant) ->
    LocalSupervisor = get_local_supervisor(Quadrant),
    io:format("🔧 SPAWN: Using supervisor ~p for ~p ~p~n", [LocalSupervisor, ObjectType, ObjectId]),
    
    case LocalSupervisor of
        slave_sup ->
            %% On slave node - use slave_sup gen_server interface
            case ObjectType of
                ball -> slave_sup:start_ball_with_id(ObjectId, InitialData);
                virus -> slave_sup:start_virus_with_id(ObjectId, InitialData);
                bomb -> slave_sup:start_bomb_with_id(ObjectId, InitialData);
                brick -> slave_sup:start_brick_with_id(ObjectId, InitialData);
                paddle -> slave_sup:start_paddle()
            end;
        FallbackSupervisor ->
            %% On main node as fallback - use supervisor:start_child directly
            {Module, Function} = case ObjectType of
                ball -> {ball, start_link};
                virus -> {virus, start_link};
                bomb -> {bomb, start_link};
                brick -> {brick, start_link};
                paddle -> {paddle, start_link}
            end,
            
            SupervisorId = case ObjectType of
                paddle -> paddle;  % Paddle is singleton
                _ -> list_to_atom(atom_to_list(ObjectType) ++ "_" ++ atom_to_list(ObjectId))
            end,
            
            Args = case ObjectType of
                paddle -> [];  % Paddle takes no args
                _ -> [ObjectId, InitialData]
            end,
            
            ObjectSpec = #{
                id => SupervisorId,
                start => {Module, Function, Args},
                restart => permanent,
                shutdown => 2000,
                type => worker,
                modules => [Module]
            },
            
            io:format("🔧 FALLBACK: Calling supervisor:start_child(~p, ...)~n", [FallbackSupervisor]),
            supervisor:start_child(FallbackSupervisor, ObjectSpec)
    end.

%% @doc Unified object stopping that works for both slave nodes and fallback
stop_object_process(ObjectType, ObjectId, Quadrant) ->
    LocalSupervisor = get_local_supervisor(Quadrant),
    io:format("🔧 STOP: Using supervisor ~p to stop ~p ~p~n", [LocalSupervisor, ObjectType, ObjectId]),
    
    case LocalSupervisor of
        slave_sup ->
            %% On slave node - use slave_sup gen_server interface
            case ObjectType of
                ball -> slave_sup:stop_ball_by_id(ObjectId);
                virus -> slave_sup:stop_virus_by_id(ObjectId);
                bomb -> slave_sup:stop_bomb_by_id(ObjectId);
                brick -> slave_sup:stop_brick_by_id(ObjectId);
                paddle -> slave_sup:stop_paddle()
            end;
        FallbackSupervisor ->
            %% On main node as fallback - use supervisor:terminate_child directly
            SupervisorId = case ObjectType of
                paddle -> paddle;  % Paddle is singleton
                _ -> list_to_atom(atom_to_list(ObjectType) ++ "_" ++ atom_to_list(ObjectId))
            end,
            
            io:format("🔧 FALLBACK: Calling supervisor:terminate_child(~p, ~p)~n", [FallbackSupervisor, SupervisorId]),
            case supervisor:terminate_child(FallbackSupervisor, SupervisorId) of
                ok -> 
                    %% Also delete the child spec to fully clean up
                    supervisor:delete_child(FallbackSupervisor, SupervisorId),
                    ok;
                Error -> Error
            end
    end.

%% Proxy loop for monitoring remote processes (legacy - for individual quadrant managers)
proxy_loop(RemotePid, Quadrant, RemoteNode) ->
    receive
        {'DOWN', _MonitorRef, process, RemotePid, Reason} ->
            io:format("FAULT DETECTED: Quadrant manager ~p on ~p died with reason: ~p~n", 
                     [Quadrant, RemoteNode, Reason]),
            
            %% Notify fault recovery manager
            fault_recovery_manager:handle_quadrant_failure(Quadrant, RemoteNode, Reason),
            
            %% Enter recovery monitoring mode instead of exiting
            proxy_recovery_loop(Quadrant, RemoteNode, Reason);
        _Other ->
            proxy_loop(RemotePid, Quadrant, RemoteNode)
    end.

%% New proxy loop for monitoring slave_sup (better fault detection)
slave_sup_proxy_loop(SlavePid, Quadrant, RemoteNode) ->
    receive
        {'DOWN', _MonitorRef, process, SlavePid, Reason} ->
            io:format("CRITICAL FAULT DETECTED!~n"),
            io:format("SLAVE_SUP DIED: slave_sup ~p on ~p died with reason: ~p~n", [SlavePid, RemoteNode, Reason]),
            io:format("AFFECTED QUADRANT: ~p will need local fallback~n", [Quadrant]),
            
            %% slave_sup died = node effectively dead = always create local fallback
            io:format(" PROXY: Calling fault_recovery_manager:handle_quadrant_failure...~n"),
            fault_recovery_manager:handle_quadrant_failure(Quadrant, RemoteNode, {slave_sup_died, Reason}),
            io:format(" PROXY: fault_recovery_manager notified!~n"),
            
            %% Enter recovery monitoring mode
            proxy_recovery_loop(Quadrant, RemoteNode, {slave_sup_died, Reason});
        _Other ->
            slave_sup_proxy_loop(SlavePid, Quadrant, RemoteNode)
    end.

%% @doc Recovery monitoring loop - keeps proxy alive during fallback operation
proxy_recovery_loop(Quadrant, FailedNode, Reason) ->
    receive
        %% TODO: Handle recovery completion signal in Step 4
        {recovery_complete, NewRemotePid} ->
            io:format("Quadrant ~p recovered, resuming normal proxy operation~n", [Quadrant]),
            _MonitorRef = monitor(process, NewRemotePid),
            proxy_loop(NewRemotePid, Quadrant, FailedNode);
        _Other ->
            %% Stay in recovery mode
            proxy_recovery_loop(Quadrant, FailedNode, Reason)
    after 30000 ->
            %% Heartbeat every 30 seconds during recovery
            io:format(" RECOVERY: Quadrant ~p still in fallback mode~n", [Quadrant]),
            proxy_recovery_loop(Quadrant, FailedNode, Reason)
    end.


%% @doc Spawn a brick process on the target quadrant
spawn_brick(TargetQuadrant, InitialData) ->
    TargetNode = get_node_from_quadrant(TargetQuadrant),
    TargetProcess = {?SERVER(TargetQuadrant), TargetNode},
    gen_server:cast(TargetProcess, {spawn_brick, InitialData}).

%% @doc Stop a specific brick by ID on the target quadrant
stop_owned_brick_by_id(TargetQuadrant, BrickId) ->
    TargetNode = get_node_from_quadrant(TargetQuadrant),
    TargetProcess = {?SERVER(TargetQuadrant), TargetNode},
    gen_server:cast(TargetProcess, {stop_brick_by_id, BrickId}).

%% @doc Send damage to a specific brick on the target quadrant
damage_brick(TargetQuadrant, BrickId) ->
    TargetNode = get_node_from_quadrant(TargetQuadrant),
    TargetProcess = {?SERVER(TargetQuadrant), TargetNode},
    gen_server:cast(TargetProcess, {damage_brick, BrickId}).

%% @doc Spawn a bomb process on the target quadrant
spawn_bomb(TargetQuadrant, InitialData) ->
    TargetNode = get_node_from_quadrant(TargetQuadrant),
    TargetProcess = {?SERVER(TargetQuadrant), TargetNode},
    gen_server:cast(TargetProcess, {spawn_bomb, InitialData}).

%% @doc Stop a specific bomb by ID on the target quadrant
stop_owned_bomb_by_id(TargetQuadrant, BombId) ->
    TargetNode = get_node_from_quadrant(TargetQuadrant),
    TargetProcess = {?SERVER(TargetQuadrant), TargetNode},
    gen_server:cast(TargetProcess, {stop_bomb_by_id, BombId}).

%% @doc Remotely spawn a virus on this quadrant's node
spawn_virus(TargetQuadrant, InitialData) ->
    TargetNode = get_node_from_quadrant(TargetQuadrant),
    TargetProcess = {?SERVER(TargetQuadrant), TargetNode},
    gen_server:cast(TargetProcess, {spawn_virus, InitialData}).

%% @doc Stop a specific virus by ID on the target quadrant
stop_owned_virus_by_id(TargetQuadrant, VirusId) ->
    TargetNode = get_node_from_quadrant(TargetQuadrant),
    TargetProcess = {?SERVER(TargetQuadrant), TargetNode},
    gen_server:cast(TargetProcess, {stop_virus_by_id, VirusId}).

%% @private Handle brick destruction and special effects
handle_brick_destruction(BrickId, State) ->
    %% Send brick destruction message to main node (which has ETS access)
    io:format("Notifying main node about brick ~p destruction~n", [BrickId]),
    gen_server:cast({game_server, get_main_node()}, {brick_destroyed, BrickId, State#state.quadrant}),
    
    %% Stop the brick process locally and remove from owned bricks
    stop_object_process(brick, BrickId, State#state.quadrant),
    NewOwnedBricks = lists:delete(BrickId, State#state.owned_bricks),
    {noreply, State#state{owned_bricks = NewOwnedBricks}}.


%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([Quadrant]) ->
    io:format("Quadrant manager ~p starting.~n", [Quadrant]),
    group_leader(whereis(user), self()),
    Boundaries = get_quadrant_boundaries(Quadrant),
    %% Disable tick timer to prevent ETS access on slave nodes - no longer true , we use the new logic
    {ok, #state{quadrant = Quadrant, boundaries = Boundaries}}.

handle_call(test_message, _From, State) ->
    io:format("Test message received on quadrant ~p, node ~p~n", [State#state.quadrant, node()]),
    {reply, {ok, test_successful, node()}, State};

handle_call({take_ownership_sync, Object}, _From, State) ->
    %% Synchronous version of take_ownership for atomic operations
    io:format(" SYNC: Taking ownership of ~p on quadrant ~p~n", [Object, State#state.quadrant]),
    case Object of
        paddle ->
            %% Same logic as async version but return success/failure
            MainNode = get_main_node(),
            case node() =:= MainNode of
                true ->
                    %% On main node - paddle already exists
                    case paddle:get_position() of
                        {ok, X} ->
                            Y = 550,
                            CurrentVel = case paddle:get_state() of
                                {ok, moving_left} -> {-10, 0};
                                {ok, moving_right} -> {10, 0};
                                {ok, idle} -> {0, 0};
                                _ -> {0, 0}
                            end,
                            gen_server:cast({game_server, MainNode}, {update_ets, paddle, {X, Y}, CurrentVel, State#state.quadrant}),
                            {reply, ok, State#state{owns_paddle = true}};
                        Error ->
                            {reply, {error, Error}, State}
                    end;
                false ->
                    %% On remote node - check if paddle process already exists first
                    case whereis(paddle) of
                        undefined ->
                            %% No paddle process exists, start new one
                            case spawn_object_process(paddle, paddle, #{}, State#state.quadrant) of
                                {ok, _Pid} ->
                                    io:format("Spawned paddle process on fallback quadrant ~p~n", [State#state.quadrant]),
                                    gen_server:cast({game_server, MainNode}, {request_paddle_position_for_ownership, State#state.quadrant}),
                                    {reply, ok, State#state{owns_paddle = true}};
                                {error, {already_started, _Pid}} ->
                                    gen_server:cast({game_server, MainNode}, {request_paddle_position_for_ownership, State#state.quadrant}),
                                    {reply, ok, State#state{owns_paddle = true}};
                                Error ->
                                    {reply, {error, Error}, State}
                            end;
                        PaddlePid when is_pid(PaddlePid) ->
                            %% Paddle process already exists, just take ownership without creating new process
                            io:format("Taking ownership of existing paddle ~p on fallback quadrant ~p~n", [PaddlePid, State#state.quadrant]),
                            gen_server:cast({game_server, MainNode}, {request_paddle_position_for_ownership, State#state.quadrant}),
                            {reply, ok, State#state{owns_paddle = true}}
                    end
            end;
        _ ->
            {reply, {error, unknown_object}, State}
    end;

handle_call({release_ownership_sync, Object, Position, Velocity}, _From, State) ->
    %% Synchronous version of release_ownership for atomic operations
    io:format(" SYNC: Releasing ownership of ~p from quadrant ~p~n", [Object, State#state.quadrant]),
    case Object of
        paddle ->
            %% Stop local paddle process and update ETS
            case stop_object_process(paddle, paddle, State#state.quadrant) of
                ok -> 
                    MainNode = get_main_node(),
                    gen_server:cast({game_server, MainNode}, {update_ets, Object, Position, Velocity, State#state.quadrant}),
                    {reply, ok, State#state{owns_paddle = false}};
                Error -> 
                    {reply, {error, Error}, State}
            end;
        _ ->
            {reply, {error, unknown_object}, State}
    end;

handle_call(_Request, _From, _State) ->
    {reply, ok, _State}.

handle_cast({take_ownership, ball}, State) ->
    % This is now primarily for logging/debugging. The presence of a ball,legacy
    % process is the real indicator of ownership.
    io:format("~n=== BALL OWNERSHIP ACKNOWLEDGED ===~n"),
    io:format("Quadrant ~p now responsible for the ball~n", [State#state.quadrant]),
    io:format("Node: ~p~n", [node()]),
    io:format("================================~n~n"),
    {noreply, State};
handle_cast({take_ownership, paddle}, State) ->%%might not need this anymore
    io:format("~n=== PADDLE OWNERSHIP TRANSFER ===~n"),
    io:format("Quadrant ~p is taking ownership of the paddle~n", [State#state.quadrant]),
    io:format("Node: ~p~n", [node()]),
    
    %% First, ensure we have a local paddle process on this node
    MainNode = get_main_node(),
    case node() =:= MainNode of
        true ->
            %% We ARE on the main node, paddle already exists
            io:format("On main node - paddle process already exists~n"),
            case paddle:get_position() of
                {ok, X} ->
                    Y = 550,  % Paddle is always at Y=550
                    io:format("Got paddle position: (~p,~p) from local paddle process~n", [X, Y]),
                    %% Get current velocity (might be 0 if stopped)
                    CurrentVel = case paddle:get_state() of
                        {ok, moving_left} -> {-10, 0};  % Use paddle speed
                        {ok, moving_right} -> {10, 0};
                        {ok, idle} -> {0, 0};
                        _ -> {0, 0}
                    end,
                    gen_server:cast({game_server, MainNode}, {update_ets, paddle, {X, Y}, CurrentVel, State#state.quadrant});
                Error ->
                    io:format("Could not get paddle position during ownership transfer: ~p~n", [Error])
            end;
        false ->
            %% We are on a REMOTE node, start a local paddle process
            io:format("Starting local paddle process on remote node~n"),
            case spawn_object_process(paddle, paddle, #{}, State#state.quadrant) of
                {ok, _Pid} ->
                    io:format("Local paddle process started successfully~n"),
                    %% Now request paddle position from main node to sync state
                    io:format("Remote node requesting paddle position from main node ~p~n", [MainNode]),
                    gen_server:cast({game_server, MainNode}, {request_paddle_position_for_ownership, State#state.quadrant});
                {error, {already_started, _Pid}} ->
                    io:format("Paddle process already running on remote node~n"),
                    gen_server:cast({game_server, MainNode}, {request_paddle_position_for_ownership, State#state.quadrant});
                Error ->
                    io:format("Failed to start paddle process: ~p~n", [Error]),
                    gen_server:cast({game_server, MainNode}, {request_paddle_position_for_ownership, State#state.quadrant})
            end
    end,
    
    io:format("==================================~n~n"),
    {noreply, State#state{owns_paddle = true}};
handle_cast(crash, State) ->
    io:format("Crashing quadrant manager ~p on purpose for testing.~n", [State#state.quadrant]),
    exit(killed_for_test);
handle_cast(stop_ball, State) ->
    io:format("Quadrant ~p stopping all owned balls~n", [State#state.quadrant]),
    %% Stop all balls and cancel their timers
    lists:foreach(fun(BallId) ->
        stop_object_process(ball, BallId, State#state.quadrant),
        case maps:get(BallId, State#state.ball_tick_timers, undefined) of
            undefined -> ok;
            Timer -> erlang:cancel_timer(Timer)
        end
    end, State#state.owned_balls),
    {noreply, State#state{owned_balls = [], ball_tick_timers = #{}}};

handle_cast({stop_ball_by_id, BallId}, State) ->
    io:format("Quadrant ~p stopping ball ~p~n", [State#state.quadrant, BallId]),
    stop_object_process(ball, BallId, State#state.quadrant),
    case maps:get(BallId, State#state.ball_tick_timers, undefined) of
        undefined -> ok;
        Timer -> erlang:cancel_timer(Timer)
    end,
    NewOwnedBalls = lists:delete(BallId, State#state.owned_balls),
    NewTimers = maps:remove(BallId, State#state.ball_tick_timers),
    {noreply, State#state{owned_balls = NewOwnedBalls, ball_tick_timers = NewTimers}};
handle_cast(launch_ball, State) ->
    io:format("Quadrant ~p launching owned balls~n", [State#state.quadrant]),
    % Launch all owned balls (for legacy compatibility)
    lists:foreach(fun(BallId) ->
        io:format("Attempting to launch ball ~p~n", [BallId]),
        % Get paddle position through centralized ownership system
        MainNode = get_main_node(),
        % Always request paddle position from main node (proper architecture)
        case gen_server:call({game_server, MainNode}, get_paddle_position, 5000) of
            {ok, {PaddleX, PaddleY}} ->
                LaunchPos = {PaddleX, PaddleY - 20},
                LaunchVel = {4, -4}, % Default launch velocity
                ball:launch(BallId, LaunchPos, LaunchVel);
            Error ->
                io:format("Cannot launch ball ~p, failed to get paddle position: ~p~n", [BallId, Error])
        end
    end, State#state.owned_balls),
    {noreply, State};
handle_cast({spawn_ball, InitialData = #{id := BallId, pos := Pos, vel := Vel}}, State) ->
    %% Spawning ball with unified supervisor detection
    
    case spawn_object_process(ball, BallId, InitialData, State#state.quadrant) of
        {ok, _Pid} ->
            %% Ball process started successfully
            ball:launch(BallId, Pos, Vel),
            NewTimer = erlang:send_after(?GAME_LOOP_INTERVAL, self(), {tick, BallId}),
            NewOwnedBalls = [BallId | State#state.owned_balls],
            NewTimers = maps:put(BallId, NewTimer, State#state.ball_tick_timers),
            {noreply, State#state{owned_balls = NewOwnedBalls, ball_tick_timers = NewTimers}};
        {error, {already_started, _Pid}} ->
            io:format("Ball ~p process already running on ~p, re-launching...~n", [BallId, node()]),
            ball:launch(BallId, Pos, Vel),
            NewTimer = erlang:send_after(?GAME_LOOP_INTERVAL, self(), {tick, BallId}),
            NewOwnedBalls = case lists:member(BallId, State#state.owned_balls) of
                true -> State#state.owned_balls;
                false -> [BallId | State#state.owned_balls]
            end,
            NewTimers = maps:put(BallId, NewTimer, State#state.ball_tick_timers),
            {noreply, State#state{owned_balls = NewOwnedBalls, ball_tick_timers = NewTimers}};
        Error ->
            io:format("Failed to start ball ~p process: ~p~n", [BallId, Error]),
            {noreply, State}
    end;

handle_cast({spawn_ball_idle, InitialData = #{id := BallId, pos := Pos}}, State) ->
    %% Spawning idle ball
    case spawn_object_process(ball, BallId, InitialData, State#state.quadrant) of
        {ok, _Pid} ->
            %% Ball process started in idle state
            %% DON'T call ball:launch() - keep ball in idle state
            %% START tick timer anyway - this might be needed for paddle movement synchronization
            NewTimer = erlang:send_after(?GAME_LOOP_INTERVAL, self(), {tick, BallId}),
            NewOwnedBalls = [BallId | State#state.owned_balls],
            NewTimers = maps:put(BallId, NewTimer, State#state.ball_tick_timers),
            {noreply, State#state{owned_balls = NewOwnedBalls, ball_tick_timers = NewTimers}};
        {error, {already_started, _Pid}} ->
            io:format("Ball ~p process already running on ~p, keeping in IDLE state~n", [BallId, node()]),
            %% START tick timer anyway for paddle movement
            NewTimer = erlang:send_after(?GAME_LOOP_INTERVAL, self(), {tick, BallId}),
            NewOwnedBalls = case lists:member(BallId, State#state.owned_balls) of
                true -> State#state.owned_balls;
                false -> [BallId | State#state.owned_balls]
            end,
            NewTimers = maps:put(BallId, NewTimer, State#state.ball_tick_timers),
            {noreply, State#state{owned_balls = NewOwnedBalls, ball_tick_timers = NewTimers}};
        Error ->
            io:format("Failed to start ball ~p process: ~p~n", [BallId, Error]),
            {noreply, State}
    end;

handle_cast({spawn_brick, InitialData = #{id := BrickId, pos := Pos, health := Health, special_effect := SpecialEffect}}, State) ->
    io:format("Quadrant ~p spawning brick ~p with data: ~p~n", [State#state.quadrant, BrickId, InitialData]),
    case spawn_object_process(brick, BrickId, InitialData, State#state.quadrant) of
        {ok, _Pid} ->
            io:format("Brick ~p process started successfully on ~p at position ~p (health=~p, effect=~p)~n", 
                     [BrickId, node(), Pos, Health, SpecialEffect]),
            %% Add brick to owned bricks list
            NewOwnedBricks = [BrickId | State#state.owned_bricks],
            {noreply, State#state{owned_bricks = NewOwnedBricks}};
        {error, {already_started, _Pid}} ->
            io:format("Brick ~p process already running on ~p~n", [BrickId, node()]),
            {noreply, State};
        Error ->
            io:format("Failed to start brick ~p process: ~p~n", [BrickId, Error]),
            {noreply, State}
    end;

handle_cast({stop_brick_by_id, BrickId}, State) ->
    io:format("Quadrant ~p stopping brick ~p~n", [State#state.quadrant, BrickId]),
    case stop_object_process(brick, BrickId, State#state.quadrant) of
        ok ->
            io:format("Brick ~p stopped successfully on ~p~n", [BrickId, node()]),
            %% Remove brick from owned bricks list
            NewOwnedBricks = lists:delete(BrickId, State#state.owned_bricks),
            {noreply, State#state{owned_bricks = NewOwnedBricks}};
        Error ->
            io:format("Failed to stop brick ~p process: ~p~n", [BrickId, Error]),
            {noreply, State}
    end;

handle_cast({damage_brick, BrickId}, State) ->
    io:format(" BRICK DAMAGE: Quadrant ~p damaging brick ~p~n", [State#state.quadrant, BrickId]),
    %% Send damage to the brick process
    case lists:member(BrickId, State#state.owned_bricks) of
        true ->
            %% We own this brick, damage it
            brick:take_damage(BrickId),
            
            %% Check if brick is destroyed and update ETS with new health
            case brick:get_health(BrickId) of
                {ok, 0} ->
                    io:format("Brick ~p destroyed, handling special effects~n", [BrickId]),
                    %% Handle brick destruction
                    handle_brick_destruction(BrickId, State);
                {ok, Health} ->
                    io:format("Brick ~p health now ~p~n", [BrickId, Health]),
                    %% Update ETS with new health so GUI shows correct color
                    %% Send message to main node to update ETS (slave nodes can't access ETS directly)
                    case brick:get_position(BrickId) of
                        {ok, Position} ->
                            %% Get special effect from brick data (we need to get it from somewhere)
                            SpecialEffect = none,  
                            %% Send update request to main node
                            MainNode = get_main_node(),
                            gen_server:cast({game_server, MainNode}, {update_ets_brick, BrickId, Position, Health, State#state.quadrant, SpecialEffect}),
                            io:format("Sent brick ~p health update to main node (~p)~n", [BrickId, MainNode]);
                        Error ->
                            io:format("Failed to get brick ~p position: ~p~n", [BrickId, Error])
                    end,
                    {noreply, State};
                Error ->
                    io:format("Failed to get brick ~p health: ~p~n", [BrickId, Error]),
                    {noreply, State}
            end;
        false ->
            io:format("Brick ~p not owned by quadrant ~p~n", [BrickId, State#state.quadrant]),
            {noreply, State}
    end;

handle_cast({spawn_bomb, InitialData = #{id := BombId, pos := Pos}}, State) ->
    io:format("Quadrant ~p spawning bomb ~p with data: ~p~n", [State#state.quadrant, BombId, InitialData]),
    case spawn_object_process(bomb, BombId, InitialData, State#state.quadrant) of
        {ok, _Pid} ->
            io:format("Bomb ~p process started successfully on ~p at position ~p~n", [BombId, node(), Pos]),
            %% Start tick timer for bomb physics
            NewTimer = erlang:send_after(?GAME_LOOP_INTERVAL, self(), {tick, BombId}),
            NewOwnedBombs = [BombId | State#state.owned_bombs],
            NewTimers = maps:put(BombId, NewTimer, State#state.bomb_tick_timers),
            {noreply, State#state{owned_bombs = NewOwnedBombs, bomb_tick_timers = NewTimers}};
        {error, {already_started, _Pid}} ->
            io:format("Bomb ~p process already running on ~p~n", [BombId, node()]),
            {noreply, State};
        Error ->
            io:format("Failed to start bomb ~p process: ~p~n", [BombId, Error]),
            {noreply, State}
    end;

handle_cast({stop_bomb_by_id, BombId}, State) ->
    io:format("Quadrant ~p stopping bomb ~p~n", [State#state.quadrant, BombId]),
    case stop_object_process(bomb, BombId, State#state.quadrant) of
        ok ->
            io:format("Bomb ~p stopped successfully on ~p~n", [BombId, node()]),
            %% Cancel timer and remove from owned bombs list
            case maps:get(BombId, State#state.bomb_tick_timers, undefined) of
                undefined -> ok;
                Timer -> erlang:cancel_timer(Timer)
            end,
            NewOwnedBombs = lists:delete(BombId, State#state.owned_bombs),
            NewTimers = maps:remove(BombId, State#state.bomb_tick_timers),
            {noreply, State#state{owned_bombs = NewOwnedBombs, bomb_tick_timers = NewTimers}};
        Error ->
            io:format("Failed to stop bomb ~p process: ~p~n", [BombId, Error]),
            {noreply, State}
    end;

handle_cast({spawn_virus, InitialData = #{id := VirusId, pos := Pos}}, State) ->
    io:format("Quadrant ~p RECEIVING virus ~p at ~p (handoff or new spawn)~n", [State#state.quadrant, VirusId, Pos]),
    case spawn_object_process(virus, VirusId, InitialData, State#state.quadrant) of
        {ok, _Pid} ->
            io:format("Virus ~p started on ~p, starting tick timer~n", [VirusId, node()]),
            %% Start tick timer for virus physics
            NewTimer = erlang:send_after(?GAME_LOOP_INTERVAL, self(), {tick, VirusId}),
            NewOwnedViruses = [VirusId | State#state.owned_viruses],
            NewTimers = maps:put(VirusId, NewTimer, State#state.virus_tick_timers),
            io:format("Quadrant ~p now owns viruses: ~p~n", [State#state.quadrant, NewOwnedViruses]),
            {noreply, State#state{owned_viruses = NewOwnedViruses, virus_tick_timers = NewTimers}};
        {error, {already_started, _Pid}} ->
            io:format("Virus ~p process already running on ~p~n", [VirusId, node()]),
            {noreply, State};
        Error ->
            io:format("Failed to start virus ~p process: ~p~n", [VirusId, Error]),
            {noreply, State}
    end;

handle_cast({stop_virus_by_id, VirusId}, State) ->
    io:format("Quadrant ~p stopping virus ~p~n", [State#state.quadrant, VirusId]),
    case stop_object_process(virus, VirusId, State#state.quadrant) of
        ok ->
            io:format("Virus ~p stopped successfully on ~p~n", [VirusId, node()]),
            %% Cancel timer and remove from owned viruses list
            case maps:get(VirusId, State#state.virus_tick_timers, undefined) of
                undefined -> ok;
                Timer -> erlang:cancel_timer(Timer)
            end,
            NewOwnedViruses = lists:delete(VirusId, State#state.owned_viruses),
            NewTimers = maps:remove(VirusId, State#state.virus_tick_timers),
            {noreply, State#state{owned_viruses = NewOwnedViruses, virus_tick_timers = NewTimers}};
        Error ->
            io:format("Failed to stop virus ~p process: ~p~n", [VirusId, Error]),
            {noreply, State}
    end;

handle_cast({move_paddle, Direction}, State) ->
    %% Forward paddle movement to the paddle gen_statem process
    case State#state.owns_paddle of
        true ->
            case Direction of
                left -> paddle:move_left();
                right -> paddle:move_right();
                Other -> io:format("Unknown paddle direction: ~p~n", [Other])
            end;
        false ->
            ok  %  ignore if we don't own the paddle
    end,
    {noreply, State};
handle_cast({stop_paddle, _Direction}, State) ->
    %% Forward paddle stop to the paddle gen_statem process
    case State#state.owns_paddle of
        true ->
            paddle:stop();
        false ->
            ok  %  ignore if we don't own the paddle
    end,
    {noreply, State};
handle_cast({update_position, Object, Position, Velocity}, State) ->
    MainNode = get_main_node(),
    gen_server:cast({game_server, MainNode}, {update_ets, Object, Position, Velocity, State#state.quadrant}),
    {noreply, State};
handle_cast({release_ownership, Object, Position, Velocity}, State) ->
    io:format("Quadrant ~p: Releasing ownership of ~p~n", [State#state.quadrant, Object]),
    
    %% Stop local process if we have one
    case Object of
        paddle ->
            io:format("Quadrant ~p: Stopping local paddle process~n", [State#state.quadrant]),
            case stop_object_process(paddle, paddle, State#state.quadrant) of
                ok -> io:format("Local paddle process stopped~n");
                Error -> io:format("Error stopping paddle: ~p~n", [Error])
            end,
            %% Update state to reflect we no longer own the paddle
            MainNode = get_main_node(),
            gen_server:cast({game_server, MainNode}, {update_ets, Object, Position, Velocity, State#state.quadrant}),
            {noreply, State#state{owns_paddle = false}};
        _ ->
            MainNode = get_main_node(),
            gen_server:cast({game_server, MainNode}, {update_ets, Object, Position, Velocity, State#state.quadrant}),
            {noreply, State}
    end;
handle_cast(_Msg, _State) ->
    {noreply, _State}.

handle_info({tick, BallId}, State = #state{quadrant = Quadrant, ball_tick_timers = Timers}) 
  when is_map_key(BallId, Timers) ->
    % Check if ball is idle first - idle balls need special handling
    case ball:get_state(BallId) of
        {ok, idle} ->
            % IDLE BALL: Sync position with current paddle position, don't do physics
            MainNode = get_main_node(),
            % Request paddle position from main node (slave nodes can't access ETS directly)
            case gen_server:call({game_server, MainNode}, get_paddle_position, 5000) of
                {ok, {PaddleX, PaddleY}} ->
                    % Calculate where idle ball should be
                    IdleBallPos = {PaddleX, PaddleY - 20}, % Ball sits 20 pixels above paddle
                    BallQuadrant = get_quadrant_from_pos(IdleBallPos),
                    
                    if BallQuadrant =/= Quadrant ->
                        % IDLE BALL HANDOFF: Ball needs to follow paddle to new quadrant
                        io:format("Ball ~p following paddle ~p -> ~p~n", [BallId, Quadrant, BallQuadrant]),
                        
                        % Use same handoff logic as moving balls but for idle state
                        case gen_server:call({game_server, MainNode}, {update_ets_sync, {ball, BallId}, IdleBallPos, {0, 0}, BallQuadrant}, 5000) of
                            ok ->
                                % Stop local ball process
                                stop_object_process(ball, BallId, Quadrant),
                                % Cancel tick timer
                                case maps:get(BallId, Timers, undefined) of
                                    undefined -> ok;
                                    Timer -> erlang:cancel_timer(Timer)
                                end,
                                % Remove from owned balls
                                NewOwnedBalls = lists:delete(BallId, State#state.owned_balls),
                                NewTimers = maps:remove(BallId, Timers),
                                % Tell new quadrant to spawn idle ball
                                timer:sleep(10),
                                spawn_ball_idle(BallQuadrant, #{id => BallId, pos => IdleBallPos, vel => {0, 0}}),
                                {noreply, State#state{owned_balls = NewOwnedBalls, ball_tick_timers = NewTimers}};
                            Error ->
                                io:format("Failed to update ETS during handoff: ~p~n", [Error]),
                                % Continue with same quadrant
                                NewTimer = erlang:send_after(?GAME_LOOP_INTERVAL, self(), {tick, BallId}),
                                NewTimers = maps:put(BallId, NewTimer, Timers),
                                {noreply, State#state{ball_tick_timers = NewTimers}}
                        end;
                    true ->
                        % SAME QUADRANT: Just update position
                        gen_server:cast({game_server, MainNode}, {update_ets, {ball, BallId}, IdleBallPos, {0, 0}, Quadrant}),
                        io:format("Ball ~p position synced to paddle at ~p~n", [BallId, IdleBallPos]),
                        % Schedule next tick
                        NewTimer = erlang:send_after(?GAME_LOOP_INTERVAL, self(), {tick, BallId}),
                        NewTimers = maps:put(BallId, NewTimer, Timers),
                        {noreply, State#state{ball_tick_timers = NewTimers}}
                    end;
                Error ->
                    io:format("Failed to get paddle position for ball ~p: ~p~n", [BallId, Error]),
                    % Schedule next tick anyway
                    NewTimer = erlang:send_after(?GAME_LOOP_INTERVAL, self(), {tick, BallId}),
                    NewTimers = maps:put(BallId, NewTimer, Timers),
                    {noreply, State#state{ball_tick_timers = NewTimers}}
            end;
        {ok, moving} ->
            % MOVING BALL: Do normal physics processing
            case ball:get_full_state(BallId) of
                {ok, {Position, Velocity}} ->
                    MainNode = get_main_node(),
                    case gen_server:call({collision_manager, MainNode}, {calculate_physics, {ball, BallId}, Position, Velocity, Quadrant}, 5000) of
                {NewPos, {NewVX, NewVY}, NewQuadrant} ->
                    % Check for ball missed signal (VY = -999)
                    if NewVY =:= -999 ->
                        io:format("Ball ~p missed detected in quadrant ~p~n", [BallId, Quadrant]),
                        % Delete ball from ETS on main node
                        gen_server:cast({game_server, MainNode}, {delete_ets_ball, BallId}),
                        % Stop the local ball process
                        stop_object_process(ball, BallId, Quadrant),
                        % Cancel the tick timer  
                        case maps:get(BallId, Timers, undefined) of
                            undefined -> ok;
                            Timer -> erlang:cancel_timer(Timer)
                        end,
                        % Remove ball from owned list and timers
                        NewOwnedBalls = lists:delete(BallId, State#state.owned_balls),
                        NewTimers = maps:remove(BallId, Timers),
                        % Note: Don't decrement life here - let game_server handle it when all balls are gone
                        {noreply, State#state{owned_balls = NewOwnedBalls, ball_tick_timers = NewTimers}};
                    true ->
                        % Normal physics processing
                        NewVel = {NewVX, NewVY},
                        
                        if NewQuadrant =/= Quadrant ->
                            % HANDOFF LOGIC - Make it atomic to prevent race conditions
                            io:format("HANDOFF DETECTED for ball ~p: ~p -> ~p at position ~p~n", [BallId, Quadrant, NewQuadrant, NewPos]),
                            
                            
                            case fault_recovery_manager:is_quadrant_failed(NewQuadrant) of
                                true ->
                                    io:format("BALL BORDER CROSSING: Target quadrant ~p is FAILED! Ball ~p trying to cross from ~p~n", 
                                            [NewQuadrant, BallId, Quadrant]);
                                false ->
                                    io:format("Target quadrant ~p is healthy, proceeding with handoff~n", [NewQuadrant])
                            end,
                            
                            % 1. First, update ETS with new quadrant ownership 
                            case gen_server:call({game_server, MainNode}, {update_ets_sync, {ball, BallId}, NewPos, NewVel, NewQuadrant}, 5000) of
                                ok ->
                                    % 2. Stop the local ball process only after ETS is updated
                                    stop_object_process(ball, BallId, Quadrant),
                                    
                                    % 3. Cancel the tick timer
                                    case maps:get(BallId, Timers, undefined) of
                                        undefined -> ok;
                                        Timer -> erlang:cancel_timer(Timer)
                                    end,
                                    
                                    % 4. Remove ball from owned list and timers
                                    NewOwnedBalls = lists:delete(BallId, State#state.owned_balls),
                                    NewTimers = maps:remove(BallId, Timers),
                                    
                                    % 5. Tell the new quadrant to spawn the ball 
                                    timer:sleep(10),  % Small delay to prevent race
                                    TargetNode = get_node_from_quadrant(NewQuadrant),
                                    %% Send ball to target quadrant
                                    spawn_ball(NewQuadrant, #{id => BallId, pos => NewPos, vel => NewVel}),
                                    
                                    {noreply, State#state{owned_balls = NewOwnedBalls, ball_tick_timers = NewTimers}};
                                Error ->
                                    io:format("Failed to update ETS during handoff for ball ~p: ~p~n", [BallId, Error]),
                                    % Continue with same quadrant ownership on failure
                                    ball:update_state(BallId, NewPos, NewVel),
                                    NewTimer = erlang:send_after(?GAME_LOOP_INTERVAL, self(), {tick, BallId}),
                                    NewTimers = maps:put(BallId, NewTimer, Timers),
                                    {noreply, State#state{ball_tick_timers = NewTimers}}
                            end;
                        true ->
                            % SAME QUADRANT: Update local first, then ETS
                            ball:update_state(BallId, NewPos, NewVel),
                            % Update ETS asynchronously for same-quadrant moves
                            gen_server:cast({game_server, MainNode}, {update_ets, {ball, BallId}, NewPos, NewVel, NewQuadrant}),
                            NewTimer = erlang:send_after(?GAME_LOOP_INTERVAL, self(), {tick, BallId}),
                            NewTimers = maps:put(BallId, NewTimer, Timers),
                            {noreply, State#state{ball_tick_timers = NewTimers}}
                        end
                    end;
        Error ->
            io:format("Failed to get ball ~p state: ~p~n", [BallId, Error]),
            NewTimer = erlang:send_after(?GAME_LOOP_INTERVAL, self(), {tick, BallId}),
            NewTimers = maps:put(BallId, NewTimer, Timers),
            {noreply, State#state{ball_tick_timers = NewTimers}}
    end;
                Error ->
                    io:format("Physics calculation error for ball ~p: ~p~n", [BallId, Error]),
                    NewTimer = erlang:send_after(?GAME_LOOP_INTERVAL, self(), {tick, BallId}),
                    NewTimers = maps:put(BallId, NewTimer, Timers),
                    {noreply, State#state{ball_tick_timers = NewTimers}}
            end;
        {error, Reason} ->
            io:format("Could not get ball ~p state: ~p. Stopping tick~n", [BallId, Reason]),
            case maps:get(BallId, Timers, undefined) of
                undefined -> ok;
                Timer -> erlang:cancel_timer(Timer)
            end,
            NewOwnedBalls = lists:delete(BallId, State#state.owned_balls),
            NewTimers = maps:remove(BallId, Timers),
            {noreply, State#state{owned_balls = NewOwnedBalls, ball_tick_timers = NewTimers}};
        Error ->
            io:format("Failed to get ball ~p state: ~p~n", [BallId, Error]),
            {noreply, State}
    end;

handle_info({tick, BombId}, State = #state{quadrant = Quadrant, bomb_tick_timers = BombTimers}) 
  when is_map_key(BombId, BombTimers) ->
    %% This is a bomb tick (BombId is in bomb_tick_timers)
    case bomb:get_position(BombId) of
        {ok, Position} ->
            case bomb:get_velocity(BombId) of
                {ok, Velocity} ->
                    %% Use collision_manager for bomb physics (similar to ball)
                    MainNode = get_main_node(),
                    case gen_server:call({collision_manager, MainNode}, {calculate_physics, {bomb, BombId}, Position, Velocity, Quadrant}, 5000) of
                        {NewPosition, NewVelocity, NewQuadrant} ->
                            %% Update bomb process with new physics
                            gen_statem:cast(list_to_atom("bomb_" ++ atom_to_list(BombId)), update_physics),
                            
                            %% Update ETS on main node for GUI sync
                            gen_server:cast({game_server, MainNode}, 
                                           {update_ets, {bomb, BombId}, NewPosition, NewVelocity, NewQuadrant}),
                            
                            %% Check if bomb hit bottom or was destroyed by collision (Y > 600 or velocity = {0,0})
                            {_X, Y} = NewPosition,
                            {VelX, VelY} = NewVelocity,
                            if Y > 600 orelse (VelX =:= 0 andalso VelY =:= 0) ->
                                io:format(" BOMB: Bomb ~p exploded/destroyed~n", [BombId]),
                                %% Stop bomb process and clean up
                                stop_object_process(bomb, BombId, Quadrant),
                                case maps:get(BombId, BombTimers, undefined) of
                                    undefined -> ok;
                                    Timer -> erlang:cancel_timer(Timer)
                                end,
                                NewOwnedBombs = lists:delete(BombId, State#state.owned_bombs),
                                NewTimers = maps:remove(BombId, BombTimers),
                                %% Remove from ETS on main node
                                gen_server:cast({game_server, MainNode}, {delete_ets_bomb, BombId}),
                                {noreply, State#state{owned_bombs = NewOwnedBombs, bomb_tick_timers = NewTimers}};
                            NewQuadrant =/= Quadrant ->
                                %%  Make it atomic like ball and virus handoff
                                io:format("BOMB HANDOFF: ~p -> ~p at position ~p~n", [Quadrant, NewQuadrant, NewPosition]),
                                
                                %% FAULT TOLERANCE DEBUG: Check if target quadrant is failed
                                case fault_recovery_manager:is_quadrant_failed(NewQuadrant) of
                                    true ->
                                        io:format("BOMB BORDER CROSSING: Target quadrant ~p is FAILED! Bomb ~p trying to cross from ~p~n", 
                                                [NewQuadrant, BombId, Quadrant]);
                                    false ->
                                        io:format("Target quadrant ~p is healthy, proceeding with handoff~n", [NewQuadrant])
                                end,
                                
                                %% 1. First, update ETS with new quadrant ownership (synchronous)
                                case gen_server:call({game_server, MainNode}, {update_ets_sync, {bomb, BombId}, NewPosition, NewVelocity, NewQuadrant}, 5000) of
                                    ok ->
                                        %% 2. Stop the local bomb process only after ETS is updated
                                        stop_object_process(bomb, BombId, Quadrant),
                                        
                                        %% 3. Cancel the tick timer
                                        case maps:get(BombId, BombTimers, undefined) of
                                            undefined -> ok;
                                            Timer -> erlang:cancel_timer(Timer)
                                        end,
                                        
                                        %% 4. Remove bomb from owned list and timers
                                        NewOwnedBombs = lists:delete(BombId, State#state.owned_bombs),
                                        NewTimers = maps:remove(BombId, BombTimers),
                                        
                                        %% 5. Tell the new quadrant to spawn the bomb (with small delay to ensure cleanup)
                                        timer:sleep(10),  % Small delay to prevent race
                                        spawn_bomb(NewQuadrant, #{id => BombId, pos => NewPosition, vel => NewVelocity}),
                                        
                                        {noreply, State#state{owned_bombs = NewOwnedBombs, bomb_tick_timers = NewTimers}};
                                    Error ->
                                        io:format("Failed to update ETS during handoff for bomb ~p: ~p~n", [BombId, Error]),
                                        %% Continue with same quadrant ownership on failure
                                        gen_statem:cast(list_to_atom("bomb_" ++ atom_to_list(BombId)), update_physics),
                                        NewTimer = erlang:send_after(?GAME_LOOP_INTERVAL, self(), {tick, BombId}),
                                        NewTimers = maps:put(BombId, NewTimer, BombTimers),
                                        {noreply, State#state{bomb_tick_timers = NewTimers}}
                                end;
                            true ->
                                %% Schedule next tick
                                NewTimer = erlang:send_after(?GAME_LOOP_INTERVAL, self(), {tick, BombId}),
                                NewTimers = maps:put(BombId, NewTimer, BombTimers),
                                {noreply, State#state{bomb_tick_timers = NewTimers}}
                            end;
                        Error ->
                            io:format("Collision manager error for bomb ~p: ~p~n", [BombId, Error]),
                            {noreply, State}
                    end;
                Error ->
                    io:format("Failed to get bomb ~p velocity: ~p~n", [BombId, Error]),
                    {noreply, State}
            end;
        Error ->
            io:format("Failed to get bomb ~p position: ~p~n", [BombId, Error]),
            {noreply, State}
    end;

handle_info({tick, VirusId}, State = #state{quadrant = Quadrant, virus_tick_timers = VirusTimers}) 
  when is_map_key(VirusId, VirusTimers) ->
    %% This is a virus tick (VirusId is in virus_tick_timers)
            %% Virus tick processing
    case virus:get_position(VirusId) of
        {ok, Position} ->
            case virus:get_velocity(VirusId) of
                {ok, Velocity} ->
                    %% Use collision_manager for virus physics
                    MainNode = get_main_node(),
                    %% Request virus physics from collision manager
                    case gen_server:call({collision_manager, MainNode}, {calculate_physics, {virus, VirusId}, Position, Velocity, Quadrant}, 5000) of
                        {_NewPosition, _NewVelocity, virus_killed} ->
                            %% Virus was killed by ball collision
                            io:format("Virus ~p KILLED by ball collision!~n", [VirusId]),
                            %% Stop virus process and clean up
                            stop_object_process(virus, VirusId, Quadrant),
                            case maps:get(VirusId, VirusTimers, undefined) of
                                undefined -> ok;
                                Timer -> erlang:cancel_timer(Timer)
                            end,
                            NewOwnedViruses = lists:delete(VirusId, State#state.owned_viruses),
                            NewTimers = maps:remove(VirusId, VirusTimers),
                            %% Notify main node to remove from ETS and award points
                            gen_server:cast({game_server, MainNode}, {virus_killed, VirusId}),
                            {noreply, State#state{owned_viruses = NewOwnedViruses, virus_tick_timers = NewTimers}};
                        {NewPosition, NewVelocity, NewQuadrant} ->
                            %% Check if we need ownership transfer FIRST
                            %% Process virus physics result
                            
                            if NewQuadrant =/= Quadrant ->
                                %% VIRUS HANDOFF LOGIC - Make it atomic like ball handoff
                                io:format("VIRUS HANDOFF: ~p -> ~p at position ~p~n", [Quadrant, NewQuadrant, NewPosition]),
                                
                                %% FAULT TOLERANCE DEBUG: Check if target quadrant is failed
                                case fault_recovery_manager:is_quadrant_failed(NewQuadrant) of
                                    true ->
                                        io:format("BORDER CROSSING: Target quadrant ~p is FAILED! Virus ~p trying to cross from ~p~n", 
                                                [NewQuadrant, VirusId, Quadrant]);
                                    false ->
                                        io:format("Target quadrant ~p is healthy, proceeding with handoff~n", [NewQuadrant])
                                end,
                                
                                %% Get full virus state including duplication timing
                                {Position, Velocity, DuplicationStartTime} = case virus:get_full_state_with_timing(VirusId) of
                                    {ok, {Pos, Vel, StartTime}} -> {Pos, Vel, StartTime};
                                    _ -> {NewPosition, NewVelocity, erlang:system_time(millisecond)}  % Fallback
                                end,
                                io:format("🕐 VIRUS HANDOFF: Preserving duplication start time ~p~n", [DuplicationStartTime]),
                                
                                %% 1. First, update ETS with new quadrant ownership (synchronous)
                                case gen_server:call({game_server, MainNode}, {update_ets_sync, {virus, VirusId}, NewPosition, NewVelocity, NewQuadrant}, 5000) of
                                    ok ->
                                        %% 2. Stop the local virus process only after ETS is updated
                                        stop_object_process(virus, VirusId, Quadrant),
                                        
                                        %% 3. Cancel the tick timer
                                        case maps:get(VirusId, VirusTimers, undefined) of
                                            undefined -> ok;
                                            Timer -> erlang:cancel_timer(Timer)
                                        end,
                                        
                                        %% 4. Remove virus from owned list and timers
                                        NewOwnedViruses = lists:delete(VirusId, State#state.owned_viruses),
                                        NewTimers = maps:remove(VirusId, VirusTimers),
                                        
                                        %% 5. Tell the new quadrant to spawn the virus (with small delay to ensure cleanup)
                                        timer:sleep(10),  % Small delay to prevent race
                                        spawn_virus(NewQuadrant, #{id => VirusId, pos => NewPosition, vel => NewVelocity, duplication_start_time => DuplicationStartTime}),
                                        
                                        {noreply, State#state{owned_viruses = NewOwnedViruses, virus_tick_timers = NewTimers}};
                                    Error ->
                                        io:format("Failed to update ETS during handoff for virus ~p: ~p~n", [VirusId, Error]),
                                        %% Continue with same quadrant ownership on failure
                                        gen_statem:cast(list_to_atom("virus_" ++ atom_to_list(VirusId)), {update_physics, NewPosition, NewVelocity}),
                                        NewTimer = erlang:send_after(?GAME_LOOP_INTERVAL, self(), {tick, VirusId}),
                                        NewTimers = maps:put(VirusId, NewTimer, VirusTimers),
                                        {noreply, State#state{virus_tick_timers = NewTimers}}
                                end;
                            true ->
                                %% Same quadrant - update local process and ETS
                                gen_statem:cast(list_to_atom("virus_" ++ atom_to_list(VirusId)), {update_physics, NewPosition, NewVelocity}),
                                
                                %% Update ETS on main node for GUI sync - get duplication start time from process
                                DuplicationStartTime = case virus:get_full_state_with_timing(VirusId) of
                                    {ok, {_, _, StartTime}} -> StartTime;
                                    _ -> erlang:system_time(millisecond)  % Fallback
                                end,
                                gen_server:cast({game_server, MainNode}, 
                                               {update_ets_virus, VirusId, NewPosition, NewVelocity, NewQuadrant, DuplicationStartTime}),
                                
                                %% Schedule next tick
                                NewTimer = erlang:send_after(?GAME_LOOP_INTERVAL, self(), {tick, VirusId}),
                                NewTimers = maps:put(VirusId, NewTimer, VirusTimers),
                                {noreply, State#state{virus_tick_timers = NewTimers}}
                            end;
                        Error ->
                            io:format("Collision manager error for virus ~p: ~p~n", [VirusId, Error]),
                            {noreply, State}
                    end;
                Error ->
                    io:format("Failed to get virus ~p velocity: ~p~n", [VirusId, Error]),
                    {noreply, State}
            end;
        Error ->
            io:format("Failed to get virus ~p position: ~p~n", [VirusId, Error]),
            {noreply, State}
    end;

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, #state{ball_tick_timers = BallTimers, bomb_tick_timers = BombTimers, virus_tick_timers = VirusTimers}) ->
    %% Cancel all ball tick timers
    maps:fold(fun(_BallId, Timer, _Acc) ->
        erlang:cancel_timer(Timer)
    end, ok, BallTimers),
    %% Cancel all bomb tick timers
    maps:fold(fun(_BombId, Timer, _Acc) ->
        erlang:cancel_timer(Timer)
    end, ok, BombTimers),
    %% Cancel all virus tick timers
    maps:fold(fun(_VirusId, Timer, _Acc) ->
        erlang:cancel_timer(Timer)
    end, ok, VirusTimers),
    ok.

code_change(_OldVsn, _State, _Extra) ->
    {ok, _State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% @private
get_quadrant_boundaries(q1) ->
    #{x_min => 0, x_max => 400, y_min => 0, y_max => 300};
get_quadrant_boundaries(q2) ->
    #{x_min => 400, x_max => 800, y_min => 0, y_max => 300};
get_quadrant_boundaries(q3) ->
    #{x_min => 0, x_max => 400, y_min => 300, y_max => 600};
get_quadrant_boundaries(q4) ->
    #{x_min => 400, x_max => 800, y_min => 300, y_max => 600}.

%% @private Determines which quadrant a coordinate belongs to.
get_quadrant_from_pos({X, Y}) when X < 400, Y < 300 -> q1;
get_quadrant_from_pos({X, Y}) when X >= 400, Y < 300 -> q2;
get_quadrant_from_pos({X, Y}) when X < 400, Y >= 300 -> q3;
get_quadrant_from_pos({X, Y}) when X >= 400, Y >= 300 -> q4.
