-module(game_server).
-behaviour(gen_server).

-export([start_link/0, start_game/0, move_paddle/1, stop_paddle/1, update_ball_state/2]).
-export([get_state/0, increment_score/1, decrement_life/0, launch_ball/0, spawn_new_ball/0, add_score/1]).
-export([get_current_score/0]). %% Score access for AOE damage
-export([request_paddle_position_for_launch/0, spawn_bricks/0]). %% Brick spawning
-export([spawn_new_virus/0]). %% Virus spawning
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).
-export([test_ball_handoff/0, test_object_structures/0, verify_brick_quadrants/0]). %% Testing functions
-export([test_spawn_bomb/0]). %% Bomb testing
-export([test_spawn_virus/0]). %% Virus testing
-export([request_ownership_transfer/4]). %% New centralized ownership transfer logic , i keep the old ones for future use

-define(PADDLE_SPEED, 5).
-define(TICK_INTERVAL, 16).
-define(AOE_DAMAGE_THRESHOLD, 1000).  %% Score needed for AOE damage
-define(AOE_BRICK_WIDTH, 80).         %% Brick width for adjacency detection
-define(AOE_BRICK_HEIGHT, 30).        %% Brick height for adjacency detection

-record(state, {
    score = 0,
    lives = 3,
    level = 1,
    tick_timer = nil,
    ball_counter = 1,    %% Track next ball ID to assign
    brick_counter = 1,   %% Track next brick ID to assign
    bomb_counter = 1,    %% Track next bomb ID to assign
    virus_counter = 1,   %% Track next virus ID to assign
    virus_limit = 5,     %% Game over when virus count exceeds this (reduced from 10)
    respawning = false   %% Flag to prevent multiple respawns
}).

%% public api
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

start_game() ->
    gen_server:cast(?MODULE, start_game).

get_state() ->
    gen_server:call(?MODULE, get_state).

increment_score(Points) ->
    gen_server:call(?MODULE, {increment_score, Points}).

decrement_life() ->
    gen_server:call(?MODULE, decrement_life).

move_paddle(Direction) ->
    gen_server:cast(?MODULE, {move_paddle, Direction}).

stop_paddle(Direction) ->
    gen_server:cast(?MODULE, {stop_paddle, Direction}).

update_ball_state(Pos, Vel) ->
    gen_server:cast(?MODULE, {update_ball_state, Pos, Vel}).

launch_ball() ->
    gen_server:cast(?MODULE, launch_ball).

spawn_new_ball() ->
    gen_server:cast(?MODULE, spawn_new_ball).

add_score(Points) ->
    gen_server:cast(?MODULE, {add_score, Points}).

%% @doc Get current score for AOE damage checking
get_current_score() ->
    gen_server:call(?MODULE, get_current_score).

spawn_new_bomb() ->
    gen_server:cast(?MODULE, spawn_new_bomb).

spawn_new_virus() ->
    gen_server:cast(?MODULE, spawn_new_virus).

%% @doc Test function to manually spawn a bomb for GUI testing
test_spawn_bomb() ->
    gen_server:cast(?MODULE, spawn_new_bomb).

%% @doc Test function to manually spawn a virus for GUI testing
test_spawn_virus() ->
    gen_server:cast(?MODULE, spawn_new_virus).

request_paddle_position_for_launch() ->
    case ets:lookup(game_objects, paddle) of
        [{paddle, {PaddleX, PaddleY}, _, _Owner}] ->
            {ok, {PaddleX, PaddleY}};
        [] ->
            {error, paddle_not_found}
    end.

test_ball_handoff() ->
    gen_server:cast(?MODULE, test_ball_handoff).

%% @doc Request ownership transfer through main node (centralized)
%% This is the ONLY way quadrants should transfer ownership - no longer true , we use the new logic
request_ownership_transfer(Object, FromQuadrant, ToQuadrant, ObjectData) ->
    gen_server:cast(?MODULE, {ownership_transfer, Object, FromQuadrant, ToQuadrant, ObjectData}).


%% gen_server callbacks
init([]) ->
    %% Create the ETS table for game objects
    ets:new(game_objects, [named_table, protected, set, {read_concurrency, true}]),
    Timer = erlang:send_after(?TICK_INTERVAL, self(), tick),
    {ok, #state{tick_timer = Timer, ball_counter = 1}}.

handle_call(get_state, _From, State) ->
    {reply, State, State};

handle_call(get_current_score, _From, State) ->
    {reply, State#state.score, State};

handle_call({increment_score, Points}, _From, State) ->
    NewScore = State#state.score + Points,
    NewState = State#state{score = NewScore},
    {reply, ok, NewState};

handle_call(decrement_life, _From, State) ->
    NewLives = State#state.lives - 1,
    {reply, ok, State#state{lives = NewLives}};

handle_call({update_ets_sync, Object, Position, Velocity, Owner}, _From, State) ->
    %% Synchronous ETS update for atomic operations during handoffs
    try
        ets:insert(game_objects, {Object, Position, Velocity, Owner}),
        {reply, ok, State}
    catch
        Error:Reason ->
            {reply, {error, {Error, Reason}}, State}
    end;

handle_call(get_paddle_position, _From, State) ->
    %% Get paddle position for slave nodes (they can't access ETS directly)
    case ets:lookup(game_objects, paddle) of
        [{paddle, {PaddleX, PaddleY}, _PaddleVel, _PaddleOwner}] ->
            {reply, {ok, {PaddleX, PaddleY}}, State};
        [] ->
            {reply, {error, paddle_not_found}, State}
    end;

handle_call(test_object_structures, _From, State) ->
    Result = do_test_object_structures(),
    {reply, Result, State};

handle_call(verify_brick_quadrants, _From, State) ->
    Result = do_verify_brick_quadrants(),
    {reply, Result, State};

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(start_game, State) ->
    
    %% Define initial positions
    BallId = list_to_atom("ball_" ++ integer_to_list(State#state.ball_counter)),
    BallPos = {400, 300},
    BallVel = {4, -4},
    PaddlePos = {400, 550},

    %% Determine initial owners
    BallOwner = get_quadrant_from_pos(BallPos),
    PaddleOwner = get_quadrant_from_pos(PaddlePos),

    %% Insert objects into the ETS table for GUI purposes, including owner
    ets:insert(game_objects, {{ball, BallId}, BallPos, BallVel, BallOwner}), 
    ets:insert(game_objects, {paddle, PaddlePos, {0, 0}, PaddleOwner}),

    %% Tell the owner quadrant to spawn the ball with ID
    quadrant_manager:spawn_ball(BallOwner, #{id => BallId, pos => BallPos, vel => BallVel}),
    quadrant_manager:take_ownership(PaddleOwner, paddle),

    %% Spawn bricks for the level
    io:format("Spawning bricks for level ~p~n", [State#state.level]),
    StateWithBricks = spawn_bricks_internal(State),

    io:format("Multi-ball game initialization complete~n"),
    {noreply, StateWithBricks#state{ball_counter = StateWithBricks#state.ball_counter + 1}};

handle_cast({move_paddle, Direction}, State) ->
    %% Use distributed messaging protocol - find which quadrant owns the paddle
    case get_current_paddle_owner() of
        {ok, OwnerQuadrant} ->
            io:format("Sending move command to quadrant ~p (direction: ~p)~n", [OwnerQuadrant, Direction]),
            quadrant_manager:move_paddle(OwnerQuadrant, Direction);
        {error, Reason} ->
            io:format("Failed to find paddle owner: ~p~n", [Reason])
    end,
    {noreply, State};
handle_cast({stop_paddle, Direction}, State) ->
    %% Use distributed messaging protocol - find which quadrant owns the paddle
    case get_current_paddle_owner() of
        {ok, OwnerQuadrant} ->
            quadrant_manager:stop_paddle(OwnerQuadrant, Direction);
        {error, Reason} ->
            io:format("Failed to stop paddle: ~p~n", [Reason])
    end,
    {noreply, State};

handle_cast({update_ets, Object, Position, Velocity, Owner}, State) ->
    %% Handle ETS updates from quadrant managers, now including ownership
    ets:insert(game_objects, {Object, Position, Velocity, Owner}),
    {noreply, State};

handle_cast({update_ets_virus, VirusId, Position, Velocity, Owner, DuplicationStartTime}, State) ->
    
    ets:insert(game_objects, {{virus, VirusId}, Position, Velocity, Owner, DuplicationStartTime}),
    {noreply, State};

handle_cast({update_ets_brick, BrickId, Position, Health, Owner, SpecialEffect}, State) ->
    %% Update ETS table with brick data (for fault tolerance recovery)
    FinalSpecialEffect = case SpecialEffect of
        none ->
            %% Try to get existing special effect from ETS
            case ets:lookup(game_objects, {brick, BrickId}) of
                [{{brick, BrickId}, _OldPos, _OldHealth, _OldOwner, ExistingEffect}] ->
                    ExistingEffect;  % Preserve existing special effect
                [] ->
                    none  % No existing brick, use none
            end;
        _ ->
            SpecialEffect  % Use provided special effect
    end,
    
    ets:insert(game_objects, {{brick, BrickId}, Position, Health, Owner, FinalSpecialEffect}),
    io:format("Updated brick ~p health to ~p, special effect: ~p~n", [BrickId, Health, FinalSpecialEffect]),
    {noreply, State};

handle_cast(spawn_bricks, State) ->
    %% Spawn bricks for the current level
    io:format("Received spawn_bricks request~n"),
    NewState = spawn_bricks_internal(State),
    {noreply, NewState};

handle_cast({add_score, Points}, State) ->
    %% Add points to the score
    NewScore = State#state.score + Points,
    io:format("Added ~p points, total score: ~p~n", [Points, NewScore]),
    {noreply, State#state{score = NewScore}};

handle_cast({brick_destroyed, BrickId, Quadrant}, State) ->
    %% Handle brick destruction on main node (has ETS access)
    io:format("Processing brick ~p destruction from quadrant ~p~n", [BrickId, Quadrant]),
    
    %% Get brick data from ETS to check special effect
    case ets:lookup(game_objects, {brick, BrickId}) of
        [{{brick, BrickId}, _Position, _Health, _Owner, SpecialEffect}] ->
            %% Award destruction points
            NewScore = State#state.score + 50,
            io:format("Added 50 points for brick ~p, total score: ~p~n", [BrickId, NewScore]),
            
            %% Handle special effects
            handle_brick_special_effect(BrickId, SpecialEffect),
            
            %% Remove brick from ETS
            ets:delete(game_objects, {brick, BrickId}),
            io:format("Removed brick ~p from game_objects table~n", [BrickId]),
            
            {noreply, State#state{score = NewScore}};
        [] ->
            io:format("Brick ~p not found in ETS for destruction~n", [BrickId]),
            {noreply, State}
    end;

handle_cast({delete_ets_ball, BallId}, State) ->
    %% Handle ball deletion from ETS when ball is missed (sent from slave quadrant managers)
    io:format("Main node deleting ball ~p from ETS~n", [BallId]),
    Key = {ball, BallId},  %% FIXED: Use correct ETS key format
    case ets:lookup(game_objects, Key) of
        [] ->
            io:format("Ball ~p not found in ETS for deletion~n", [BallId]);
        [Entry] ->
            io:format("Deleting ball ~p: ~p~n", [BallId, Entry]),
            ets:delete(game_objects, Key)
    end,
    {noreply, State};

handle_cast({delete_ets_bomb, BombId}, State) ->
    %% Handle bomb deletion from ETS when bomb explodes (sent from slave quadrant managers)
    io:format("Main node deleting bomb ~p from ETS~n", [BombId]),
    Key = {bomb, BombId},
    case ets:lookup(game_objects, Key) of
        [] ->
            io:format("Bomb ~p not found in ETS for deletion~n", [BombId]);
        [Entry] ->
            io:format("Deleting bomb ~p: ~p~n", [BombId, Entry]),
            ets:delete(game_objects, Key)
    end,
    {noreply, State};

handle_cast({virus_killed, VirusId}, State) ->
    %% Handle virus death from ball collision (sent from virus processes)
    io:format("Main node processing virus ~p death~n", [VirusId]),
    Key = {virus, VirusId},
    case ets:lookup(game_objects, Key) of
        [] ->
            io:format("Virus ~p not found in ETS for deletion~n", [VirusId]),
            {noreply, State};
        [Entry] ->
            io:format("Deleting virus ~p: ~p~n", [VirusId, Entry]),
            ets:delete(game_objects, Key),
            %% Award points for killing virus
            NewScore = State#state.score + 100,
            io:format("Added 100 points for virus ~p, total score: ~p~n", [VirusId, NewScore]),
            {noreply, State#state{score = NewScore}}
    end;

handle_cast({duplicate_virus, OriginalVirusId, Position}, State) ->
    %% Handle virus duplication request
    io:format("Duplicating virus ~p at position ~p~n", [OriginalVirusId, Position]),
    
    %% Check virus limit FIRST
    AllObjects = ets:tab2list(game_objects),
    VirusObjects = [Obj || Obj <- AllObjects, element(1, element(1, Obj)) == virus],
    CurrentVirusCount = length(VirusObjects),
    
    %% Check virus count against limit
    
    if CurrentVirusCount >= State#state.virus_limit ->
        %% Virus duplication blocked by limit
        {noreply, State};
    true ->
        %% Create duplicate virus near original position
        VirusId = list_to_atom("virus_" ++ integer_to_list(State#state.virus_counter)),
        
        %% Spawn near original virus with slight offset
        {OrigX, OrigY} = Position,
        OffsetX = rand:uniform(40) - 20, % -20 to +20
        OffsetY = rand:uniform(40) - 20, % -20 to +20
        VirusPos = {max(50, min(750, OrigX + OffsetX)), max(100, min(500, OrigY + OffsetY))},
        
        %% Random velocity for new virus
        VelX = (rand:uniform(6) - 3),
        VelY = (rand:uniform(6) - 3),
        VelX2 = if VelX == 0 -> 2; abs(VelX) < 1 -> if VelX < 0 -> -2; true -> 2 end; true -> VelX end,
        VelY2 = if VelY == 0 -> 2; abs(VelY) < 1 -> if VelY < 0 -> -2; true -> 2 end; true -> VelY end,
        VirusVel = {VelX2, VelY2},
        
        %% Determine quadrant
        RandomQuadrant = get_quadrant_from_pos(VirusPos),
        
        %% Insert into ETS with duplication timing info (for crash tolerance)
        DuplicationStartTime = erlang:system_time(millisecond),
        ets:insert(game_objects, {{virus, VirusId}, VirusPos, VirusVel, RandomQuadrant, DuplicationStartTime}),
        quadrant_manager:spawn_virus(RandomQuadrant, #{id => VirusId, pos => VirusPos, vel => VirusVel, duplication_start_time => DuplicationStartTime}),
        
        io:format("New virus ~p spawned at ~p on quadrant ~p~n", [VirusId, VirusPos, RandomQuadrant]),
        {noreply, State#state{virus_counter = State#state.virus_counter + 1}}
    end;

handle_cast({request_paddle_position_for_ownership, RequestingQuadrant}, State) ->
    %% Remote quadrant requesting paddle position for ownership transfer
    io:format("Quadrant ~p requesting paddle ownership (possible fallback)~n", [RequestingQuadrant]),
    case paddle:get_position() of
        {ok, X} ->
            Y = 550,  % Paddle is always at Y=550
            io:format("Got paddle position: (~p,~p), updating ETS with owner ~p~n", [X, Y, RequestingQuadrant]),
            %% Get current velocity
            CurrentVel = case paddle:get_state() of
                {ok, moving_left} -> {-10, 0};
                {ok, moving_right} -> {10, 0};
                {ok, idle} -> {0, 0};
                _ -> {0, 0}
            end,
            %% Update ETS with new ownership
            ets:insert(game_objects, {paddle, {X, Y}, CurrentVel, RequestingQuadrant});
        Error ->
            io:format("Could not get paddle position for ownership request: ~p~n", [Error])
    end,
    {noreply, State};

handle_cast({update_ball_state, _Pos, _Vel}, State) ->
    %% Legacy function - now we manage multi-ball system
    io:format("DEPRECATED: update_ball_state called - use multi-ball system instead~n"),
    {noreply, State};

handle_cast(decrement_life, State = #state{tick_timer = Timer}) ->
    %% Only decrement if not already respawning
    case State#state.respawning of
        true ->
            io:format("Already respawning, ignoring duplicate decrement~n"),
            {noreply, State};
        false ->
            NewLives = State#state.lives - 1,
            if 
                NewLives =< 0 ->
                    io:format("GAME OVER! No lives remaining!~n"),
                    %% Delete all balls from ETS
                    AllObjects = ets:tab2list(game_objects),
                    BallObjects = lists:filter(fun
                        ({{ball, _BallId}, _Pos, _Vel, _Owner}) -> true;
                        (_) -> false
                    end, AllObjects),
                    lists:foreach(fun({{ball, BallId}, _Pos, _Vel, _Owner}) ->
                        ets:delete(game_objects, {{ball, BallId}})
                    end, BallObjects),
                    gui:game_over(),
                    if 
                        Timer =/= nil -> erlang:cancel_timer(Timer);
                        true -> ok
                    end,
                    {noreply, State#state{lives = NewLives, tick_timer = nil}};
                true ->
                    io:format("Life lost! Lives remaining: ~p~n", [NewLives]),
                    erlang:send_after(1000, self(), respawn_ball),
                    {noreply, State#state{lives = NewLives, respawning = true}}  %% Set respawning flag
            end
    end;

handle_cast(launch_ball, State) ->
    io:format("Game server received launch command~n"),
    %% Instead of only launching from paddle's quadrant, broadcast to all quadrants
    %% This fixes the issue where ball might be owned by a different quadrant than paddle - i dont think this is needed anymore
    io:format("Broadcasting launch command to all quadrants~n"),
    quadrant_manager:launch_owned_ball(q1),
    quadrant_manager:launch_owned_ball(q2), 
    quadrant_manager:launch_owned_ball(q3),
    quadrant_manager:launch_owned_ball(q4),
    {noreply, State};

handle_cast(spawn_new_ball, State) ->
    io:format("Spawning new ball (ID counter: ~p)~n", [State#state.ball_counter]),
    
    %% Generate unique ball ID
    BallId = list_to_atom("ball_" ++ integer_to_list(State#state.ball_counter)),
    
    %% Generate random position in a random quadrant
    Quadrants = [q1, q2, q3, q4],
    RandomQuadrant = lists:nth(rand:uniform(length(Quadrants)), Quadrants),
    
    %% Generate random position within the selected quadrant
    {MinX, MaxX, MinY, MaxY} = case RandomQuadrant of
        q1 -> {50, 350, 100, 250};   % Top-left quadrant with margins
        q2 -> {450, 750, 100, 250};  % Top-right quadrant with margins
        q3 -> {50, 350, 350, 500};   % Bottom-left quadrant with margins
        q4 -> {450, 750, 350, 500}   % Bottom-right quadrant with margins
    end,
    
    RandomX = MinX + rand:uniform(MaxX - MinX),
    RandomY = MinY + rand:uniform(MaxY - MinY),
    BallPos = {RandomX, RandomY},
    
    %% Generate random velocity
    VelX = (rand:uniform(8) - 4), % -4 to 4
    VelY = (rand:uniform(8) - 4), % -4 to 4
    %% Ensure minimum velocity
    VelX2 = if VelX == 0 -> 3; abs(VelX) < 2 -> if VelX < 0 -> -3; true -> 3 end; true -> VelX end,
    VelY2 = if VelY == 0 -> -3; abs(VelY) < 2 -> if VelY < 0 -> -3; true -> 3 end; true -> VelY end,
    BallVel = {VelX2, VelY2},
    
    io:format("New ball ~p spawning in quadrant ~p at ~p with velocity ~p~n", 
              [BallId, RandomQuadrant, BallPos, BallVel]),
    
    %% Insert into ETS table
    ets:insert(game_objects, {{ball, BallId}, BallPos, BallVel, RandomQuadrant}),
    
    %% Tell the quadrant to spawn the ball
    quadrant_manager:spawn_ball(RandomQuadrant, #{id => BallId, pos => BallPos, vel => BallVel}),
    
    io:format("Ball ~p spawned successfully!~n", [BallId]),
    {noreply, State#state{ball_counter = State#state.ball_counter + 1}};

handle_cast(spawn_new_bomb, State) ->
    io:format("Spawning new bomb (ID counter: ~p)~n", [State#state.bomb_counter]),
    
    %% Generate unique bomb ID
    BombId = list_to_atom("bomb_" ++ integer_to_list(State#state.bomb_counter)),
    
    %% Random spawn position at top of screen
    BombX = 100 + rand:uniform(600), % Random X between 100-700
    BombY = 50,  % Start near top
    BombPos = {BombX, BombY},
    BombVel = {0, 3}, % Fall straight down at speed 3
    
    %% Determine which quadrant owns this bomb
    RandomQuadrant = get_quadrant_from_pos(BombPos),
    
    %% Insert bomb into ETS for GUI
    ets:insert(game_objects, {{bomb, BombId}, BombPos, BombVel, RandomQuadrant}),
    
    %% Spawn bomb process on appropriate slave node
    quadrant_manager:spawn_bomb(RandomQuadrant, #{id => BombId, pos => BombPos, vel => BombVel}),
    
    io:format("Bomb ~p spawned successfully at ~p on quadrant ~p~n", [BombId, BombPos, RandomQuadrant]),
    {noreply, State#state{bomb_counter = State#state.bomb_counter + 1}};

handle_cast(spawn_new_virus, State) ->
    io:format("Spawning new virus (ID counter: ~p)~n", [State#state.virus_counter]),
    
    %% Check current virus count to prevent spam
    AllObjects = ets:tab2list(game_objects),
    VirusObjects = [Obj || Obj <- AllObjects, element(1, element(1, Obj)) == virus],
    CurrentVirusCount = length(VirusObjects),
    
    if CurrentVirusCount >= State#state.virus_limit ->
        io:format("Virus spawn blocked: Already at virus limit (~p/~p)!~n", 
                 [CurrentVirusCount, State#state.virus_limit]),
        {noreply, State};
    true ->
    
    %% Generate unique virus ID
    VirusId = list_to_atom("virus_" ++ integer_to_list(State#state.virus_counter)),
    
    %% Random spawn position anywhere on screen
    VirusX = 50 + rand:uniform(700), % Random X between 50-750
    VirusY = 100 + rand:uniform(400), % Random Y between 100-500
    VirusPos = {VirusX, VirusY},
    
    %% Random velocity for virus movement
    VelX = (rand:uniform(6) - 3), % -3 to 3
    VelY = (rand:uniform(6) - 3), % -3 to 3
    %% Ensure minimum velocity
    VelX2 = if VelX == 0 -> 2; abs(VelX) < 1 -> if VelX < 0 -> -2; true -> 2 end; true -> VelX end,
    VelY2 = if VelY == 0 -> 2; abs(VelY) < 1 -> if VelY < 0 -> -2; true -> 2 end; true -> VelY end,
    VirusVel = {VelX2, VelY2},
    
    %% Determine which quadrant owns this virus
    RandomQuadrant = get_quadrant_from_pos(VirusPos),
    
    %% Insert virus into ETS with duplication timing info (for crash tolerance)
    DuplicationStartTime = erlang:system_time(millisecond),
    ets:insert(game_objects, {{virus, VirusId}, VirusPos, VirusVel, RandomQuadrant, DuplicationStartTime}),
    
    %% Spawn virus process on appropriate slave node
    quadrant_manager:spawn_virus(RandomQuadrant, #{id => VirusId, pos => VirusPos, vel => VirusVel, duplication_start_time => DuplicationStartTime}),
    
    io:format("Virus ~p spawned successfully at ~p on quadrant ~p~n", [VirusId, VirusPos, RandomQuadrant]),
    {noreply, State#state{virus_counter = State#state.virus_counter + 1}}
    end;
 
handle_cast(test_ball_handoff, State) ->
    io:format("--- Multi-ball: Initiating Ball Handoff Test ---~n"),
    
    InitialPos = {398, 200},
    InitialVel = {5, 0},
    BallId = list_to_atom("test_ball_" ++ integer_to_list(State#state.ball_counter)),
    InitialOwner = get_quadrant_from_pos(InitialPos),

    io:format("Setting test ball ~p initial state. Position: ~p, Velocity: ~p~n", [BallId, InitialPos, InitialVel]),
    
    % 4. Insert into ETS and spawn ball
    ets:insert(game_objects, {{ball, BallId}, InitialPos, InitialVel, InitialOwner}),
    quadrant_manager:spawn_ball(InitialOwner, #{id => BallId, pos => InitialPos, vel => InitialVel}),
    
    
    
    {noreply, State#state{ball_counter = State#state.ball_counter + 1}};

handle_cast({ownership_transfer, Object, FromQuadrant, ToQuadrant, ObjectData}, State) ->
    %% CENTRALIZED OWNERSHIP TRANSFER - All ownership changes go through main node - no longer true , we use the new logic
    
    %% Extract object data once
    Position = maps:get(pos, ObjectData, {0, 0}),
    Velocity = maps:get(vel, ObjectData, {0, 0}),
    
    %% Step 1: Tell the old quadrant to release ownership
    io:format("Step 1: Telling ~p to release ~p ownership~n", [FromQuadrant, Object]),
    quadrant_manager:release_ownership(FromQuadrant, Object, Position, Velocity),
    
    %% Step 2: Update ETS with new owner
    ets:insert(game_objects, {Object, Position, Velocity, ToQuadrant}),
    io:format("ETS Updated: {~p, ~p, ~p, ~p}~n", [Object, Position, Velocity, ToQuadrant]),
    
    %% Step 3: Tell the new quadrant to take ownership
    io:format("Step 3: Telling ~p to take ~p ownership~n", [ToQuadrant, Object]),
    case Object of
        ball ->
            quadrant_manager:spawn_ball(ToQuadrant, ObjectData);
        {virus, VirusId} ->
            io:format("Transferring virus ~p to quadrant ~p with data ~p~n", [VirusId, ToQuadrant, ObjectData]),
            quadrant_manager:spawn_virus(ToQuadrant, ObjectData);
        paddle ->
            quadrant_manager:take_ownership(ToQuadrant, Object)
    end,
    
    io:format("Ownership transfer completed successfully!~n"),
    {noreply, State};

handle_cast(_Cast, State) ->
    {noreply, State}.

handle_info(tick, State) ->
    %% Check if game is over (no lives left)
    if 
        State#state.lives =< 0 ->
            %% Game over - don't update game state or schedule next tick
            io:format("Game stopped - no lives remaining~n"),
            {noreply, State};
        true ->
            %% The main game server tick no longer drives the ball directly.
            %% It's only responsible for things like GUI updates and checking game rules.
            cleanup_fallen_balls(),  %% Clean up fallen balls FIRST
            
            %% Check if all balls are gone - but only if not already respawning
            case State#state.respawning of
                false ->
                    %% Get all ball entries from ETS
                    AllObjects = ets:tab2list(game_objects),
                    BallObjects = lists:filter(fun
                        ({{ball, _BallId}, _Pos, _Vel, _Owner}) -> true;
                        (_) -> false
                    end, AllObjects),
                    
                    
                    case BallObjects of
                        [] ->
                            %% Check if we have a paddle (indicates game has started)
                            case ets:lookup(game_objects, paddle) of
                                [{paddle, _, _, _}] ->
                                    %% Game has started but no balls remain
                                    io:format("All balls lost! Decrementing life~n"),
                                    gen_server:cast(?MODULE, decrement_life);
                                [] ->
                                    %% No paddle means game hasn't started yet, don't decrement life
                                    
                                    ok
                            end;
                        _ ->
                            
                            ok
                    end;
                true ->
                    
                    ok
            end,
            
            %% 2. Send tick to gen_statem processes (if they exist for paddle, etc.)
            tick_paddle(),
            
            %% 3. Schedule next tick
            NewTimer = erlang:send_after(?TICK_INTERVAL, self(), tick),
            {noreply, State#state{tick_timer = NewTimer}}
    end;

handle_info(respawn_ball, State) ->
    %% Don't respawn if game is over
    if State#state.lives =< 0 ->
        io:format("Game over - not respawning ball~n"),
        {noreply, State#state{respawning = false}};
    true ->
        io:format("Respawning ball... (current counter: ~p)~n", [State#state.ball_counter]),
        case ets:lookup(game_objects, paddle) of
        [{paddle, {PaddleX, PaddleY}, _, PaddleOwner}] ->
            BallId = list_to_atom("ball_" ++ integer_to_list(State#state.ball_counter)),
            NewPos = {PaddleX, PaddleY - 20}, % Start just above the paddle
            NewVel = {0,0}, % Start idle - ball will be in idle state waiting for launch
            ets:insert(game_objects, {{ball, BallId}, NewPos, NewVel, PaddleOwner}),
            %% Don't launch the ball yet - just spawn it in idle state
            quadrant_manager:spawn_ball_idle(PaddleOwner, #{id => BallId, pos => NewPos, vel => NewVel}),
            io:format("Ball ~p respawned at paddle position~n", [BallId]),
            {noreply, State#state{ball_counter = State#state.ball_counter + 1, respawning = false}};  %% Clear respawning flag
        [] ->
            io:format("Cannot respawn ball, paddle not found~n"),
            {noreply, State#state{respawning = false}}  %% Clear respawning flag even on failure
        end
    end;

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.



%%%===================================================================
%%% Internal functions
%%%===================================================================

cleanup_fallen_balls() ->
    %% Simple, reliable cleanup: check ETS directly and delete fallen balls
    AllObjects = ets:tab2list(game_objects),
    BallObjects = lists:filter(fun
        ({{ball, _BallId}, _Pos, _Vel, _Owner}) -> true;
        (_) -> false
    end, AllObjects),
    
    
    
    %% Delete balls that have fallen below the screen
    lists:foreach(fun({{ball, BallId}, {_X, Y}, _Vel, Owner}) ->
        if Y > 578 ->
            Key = {ball, BallId},  %% FIXED: Single tuple, not double-wrapped
            
            case ets:lookup(game_objects, Key) of
                [] ->
                    ok; %% Ball already gone
                [_Entry] ->
                    ets:delete(game_objects, Key),
                    ok 
            end,
            %% Also tell the owning quadrant to stop the ball process
            quadrant_manager:stop_owned_ball_by_id(Owner, BallId);
        true ->
            ok
        end
    end, BallObjects).


%% @private Send tick to all gen_statem game objects
tick_paddle() ->
    %% Send tick to paddle if it exists  
    case whereis(paddle) of
        undefined -> ok;
        _PaddlePid -> gen_statem:cast(paddle, tick)
    end.



%% @private Get the current paddle owner from ETS table
get_current_paddle_owner() ->
    case ets:lookup(game_objects, paddle) of
        [{paddle, Position, _Velocity, Owner}] ->
            io:format("Found paddle owner ~p at position ~p~n", [Owner, Position]),
            {ok, Owner};
        [{paddle, Position, _Velocity}] ->
            %% Legacy format without owner - determine from position
            Owner = get_quadrant_from_pos(Position),
            io:format("Legacy paddle format, determined owner ~p from position ~p~n", [Owner, Position]),
            {ok, Owner};
        [] ->
            io:format("Paddle not found in ETS!~n"),
            {error, paddle_not_found};
        Other ->
            io:format("Unexpected paddle data in ETS: ~p~n", [Other]),
            {error, invalid_paddle_data}
    end.

%% @private Determines which quadrant a coordinate belongs to.
%% TODO: These values should come from a config file.
get_quadrant_from_pos({X, Y}) when X < 400, Y < 300 -> q1;
get_quadrant_from_pos({X, Y}) when X >= 400, Y < 300 -> q2;
get_quadrant_from_pos({X, Y}) when X < 400, Y >= 300 -> q3;
get_quadrant_from_pos({X, Y}) when X >= 400, Y >= 300 -> q4.

%% @private Handle special effects when brick is destroyed
handle_brick_special_effect(BrickId, none) ->
    io:format("Brick ~p destroyed with no special effect~n", [BrickId]);
handle_brick_special_effect(BrickId, spawn_ball) ->
    io:format("Brick ~p spawning new ball!~n", [BrickId]),
    spawn_new_ball();
handle_brick_special_effect(BrickId, spawn_bomb) ->
    io:format("Brick ~p spawning bomb!~n", [BrickId]),
    spawn_new_bomb();
handle_brick_special_effect(BrickId, virus) ->
    io:format("Brick ~p spawning virus!~n", [BrickId]),
    spawn_new_virus().

%% @doc Spawn a grid of bricks for the level
spawn_bricks() ->
    gen_server:cast(?MODULE, spawn_bricks).

%% @private Internal function to spawn bricks within game_server process
spawn_bricks_internal(State) ->
    io:format("Spawning brick grid for level ~p~n", [State#state.level]),
    
    %% Brick grid configuration
    Columns = 8,
    Rows = 5,
    BrickWidth = 80,
    BrickHeight = 30,
    StartX = 80,  %% Left margin
    StartY = 80,  %% Top margin
    
    %% Special effects distribution for randomization
    SpecialEffects = [none, none, none, none, none, none, none, spawn_ball, spawn_ball, spawn_bomb, spawn_bomb, virus],
    
    %% Generate all brick positions and properties
    BrickData = lists:flatten([
        [begin
            X = StartX + (Col * BrickWidth),
            Y = StartY + (Row * BrickHeight),
            Position = {X, Y},
            
            %% Generate brick properties
            BrickId = list_to_atom("brick_" ++ integer_to_list(State#state.brick_counter + (Row * Columns) + Col)),
            Health = rand:uniform(4),  %% Random health 1-4
            Owner = get_quadrant_from_pos(Position),
            SpecialEffect = lists:nth(rand:uniform(length(SpecialEffects)), SpecialEffects),
            
            %% Create brick entry
            BrickEntry = {{brick, BrickId}, Position, Health, Owner, SpecialEffect},
            
            %% Brick assigned to quadrant
            
            BrickEntry
        end || Col <- lists:seq(0, Columns-1)]
    || Row <- lists:seq(0, Rows-1)]),
    
    %% Insert all bricks into ETS AND spawn brick processes on slave nodes
    lists:foreach(fun({{brick, BrickId}, Position, Health, Owner, SpecialEffect}) ->
        %% Insert into ETS
        BrickEntry = {{brick, BrickId}, Position, Health, Owner, SpecialEffect},
        ets:insert(game_objects, BrickEntry),
        
        %% Spawn brick process on the appropriate slave node
        InitialData = #{
            id => BrickId,
            pos => Position,
            health => Health,
            special_effect => SpecialEffect
        },
        
        io:format("Spawning brick process ~p on quadrant ~p~n", [BrickId, Owner]),
        quadrant_manager:spawn_brick(Owner, InitialData)
    end, BrickData),
    
    %% Update brick counter
    NewBrickCounter = State#state.brick_counter + length(BrickData),
    
    io:format("Spawned ~p bricks successfully!~n", [length(BrickData)]),
    
    State#state{brick_counter = NewBrickCounter}.

%% @doc Test function to verify new object structures work with ETS
test_object_structures() ->
    gen_server:call(?MODULE, test_object_structures).

%% @private Internal test function that runs within game_server process
do_test_object_structures() ->
    io:format("Testing: Object structures and ETS integration~n"),
    
    %% Test brick structure
    BrickId = brick_1,
    BrickPos = {100, 150},
    BrickHealth = 4,
    BrickOwner = q1,
    BrickEffect = spawn_ball,
    BrickEntry = {{brick, BrickId}, BrickPos, BrickHealth, BrickOwner, BrickEffect},
    
    %% Test virus structure  
    VirusId = virus_1,
    VirusPos = {200, 250},
    VirusVel = {3, -2},
    VirusOwner = q2,
    VirusEntry = {{virus, VirusId}, VirusPos, VirusVel, VirusOwner},
    
    %% Test bomb structure
    BombId = bomb_1,
    BombPos = {300, 350},
    BombVel = {0, 4},
    BombOwner = q3,
    BombEntry = {{bomb, BombId}, BombPos, BombVel, BombOwner},
    
    %% Insert test objects into ETS
    ets:insert(game_objects, BrickEntry),
    ets:insert(game_objects, VirusEntry),
    ets:insert(game_objects, BombEntry),
    
    %% Verify retrieval
    case ets:lookup(game_objects, {brick, BrickId}) of
        [RetrievedBrick] ->
            io:format("BRICK: Successfully stored and retrieved: ~p~n", [RetrievedBrick]);
        [] ->
            io:format("BRICK: Failed to retrieve from ETS~n")
    end,
    
    case ets:lookup(game_objects, {virus, VirusId}) of
        [RetrievedVirus] ->
            io:format("VIRUS: Successfully stored and retrieved: ~p~n", [RetrievedVirus]);
        [] ->
            io:format("VIRUS: Failed to retrieve from ETS~n")
    end,
    
    case ets:lookup(game_objects, {bomb, BombId}) of
        [RetrievedBomb] ->
            io:format("BOMB: Successfully stored and retrieved: ~p~n", [RetrievedBomb]);
        [] ->
            io:format("BOMB: Failed to retrieve from ETS~n")
    end,
    
    %% Test socket server formatting
    TestObjects = [BrickEntry, VirusEntry, BombEntry],
    io:format("Testing format_object function~n"),
    lists:foreach(fun(Obj) ->
        try
            FormattedObj = socket_server:format_object(Obj),
            io:format("FORMATTED: ~p -> ~p~n", [element(1, Obj), FormattedObj])
        catch
            Error:Reason ->
                io:format("FORMAT ERROR: ~p - ~p:~p~n", [element(1, Obj), Error, Reason])
        end
    end, TestObjects),
    
    %% Clean up test objects
    ets:delete(game_objects, {brick, BrickId}),
    ets:delete(game_objects, {virus, VirusId}),
    ets:delete(game_objects, {bomb, BombId}),
    
    io:format("Object structure test completed~n"),
    ok.

%% @doc Verify brick quadrant assignments are correct
verify_brick_quadrants() ->
    gen_server:call(?MODULE, verify_brick_quadrants).

%% @private Internal function to verify brick quadrant assignments
do_verify_brick_quadrants() ->
    io:format("Checking brick quadrant assignments~n"),
    
    %% Get all brick entries from ETS
    AllObjects = ets:tab2list(game_objects),
    BrickObjects = lists:filter(fun
        ({{brick, _BrickId}, _Pos, _Health, _Owner, _Effect}) -> true;
        (_) -> false
    end, AllObjects),
    
    %% Count bricks per quadrant and verify positions
    QuadrantCounts = #{q1 => 0, q2 => 0, q3 => 0, q4 => 0},
    
    {FinalCounts, Errors} = lists:foldl(fun({{brick, BrickId}, {X, Y}, _Health, Owner, _Effect}, {Counts, ErrorList}) ->
        %% Calculate expected quadrant based on position
        ExpectedQuadrant = get_quadrant_from_pos({X, Y}),
        
        %% Check if assignment matches
        case Owner of
            ExpectedQuadrant ->
                %% Correct assignment
                NewCount = maps:get(Owner, Counts) + 1,
                NewCounts = maps:put(Owner, NewCount, Counts),
                io:format("BRICK ~p at {~p,~p} -> ~p (correct)~n", [BrickId, X, Y, Owner]),
                {NewCounts, ErrorList};
            _ ->
                %% Incorrect assignment
                io:format("BRICK ~p at {~p,~p} -> ~p (expected ~p)~n", [BrickId, X, Y, Owner, ExpectedQuadrant]),
                {Counts, [BrickId | ErrorList]}
        end
    end, {QuadrantCounts, []}, BrickObjects),
    
    
    
    case Errors of
        [] ->
            io:format("All brick quadrant assignments are CORRECT!~n");
        _ ->
            io:format("Found ~p assignment errors: ~p~n", [length(Errors), Errors])
    end,
    
    ok.
