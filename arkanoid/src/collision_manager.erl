-module(collision_manager).
-behaviour(gen_server).

-export([start_link/0]).
-export([start_game/0, calculate_physics/4]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% public api
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% Asynchronously start a new game.
start_game() ->
    gen_server:cast(?MODULE, start_game).

%% @doc Calculate physics for an object (collision detection, new position/velocity)
calculate_physics(Object, CurrentPos, CurrentVel, RequestingQuadrant) ->
    gen_server:call(?MODULE, {calculate_physics, Object, CurrentPos, CurrentVel, RequestingQuadrant}).

%% gen_server callbacks
init([]) ->
    {ok, #{}}.

handle_call({calculate_physics, {ball, _BallId}, {X, Y}, {VX, VY}, _RequestingQuadrant}, _From, State) ->
    %% Central physics engine for ball collision detection with ball ID
    %% Calculate new position
    NewX = X + VX,
    NewY = Y + VY,
    
    %% Handle all collision types
    {FinalX, FinalY, FinalVX, FinalVY} = handle_all_collisions(NewX, NewY, VX, VY),
    
    %% Determine which quadrant the ball should be in
    NewQuadrant = get_quadrant_from_pos({FinalX, FinalY}),
    
    Result = {
        {FinalX, FinalY},      % New position
        {FinalVX, FinalVY},    % New velocity
        NewQuadrant            % New owner quadrant
    },
    
    {reply, Result, State};

handle_call({calculate_physics, ball, {X, Y}, {VX, VY}, _RequestingQuadrant}, _From, State) ->
    %% Legacy support for single ball (backward compatibility)
    %% Calculate new position
    NewX = X + VX,
    NewY = Y + VY,
    
    %% Handle all collision types
    {FinalX, FinalY, FinalVX, FinalVY} = handle_all_collisions(NewX, NewY, VX, VY),
    
    %% Determine which quadrant the ball should be in
    NewQuadrant = get_quadrant_from_pos({FinalX, FinalY}),
    
    Result = {
        {FinalX, FinalY},      % New position
        {FinalVX, FinalVY},    % New velocity
        NewQuadrant            % New owner quadrant
    },
    
    {reply, Result, State};

handle_call({calculate_physics, {bomb, _BombId}, {X, Y}, {VX, VY}, _RequestingQuadrant}, _From, State) ->
    %% Bomb physics calculation (simple falling + paddle collision)
    NewX = X + VX,
    NewY = Y + VY,
    
    %% Check bomb-paddle collision
    {FinalX, FinalY, FinalVX, FinalVY} = handle_bomb_collisions(NewX, NewY, VX, VY),
    
    %% Determine which quadrant the bomb should be in
    NewQuadrant = get_quadrant_from_pos({FinalX, FinalY}),
    
    Result = {
        {FinalX, FinalY},      % New position
        {FinalVX, FinalVY},    % New velocity
        NewQuadrant            % New owner quadrant
    },
    
    {reply, Result, State};

handle_call({calculate_physics, {virus, VirusId}, {X, Y}, {VX, VY}, RequestingQuadrant}, _From, State) ->
    %% Virus physics calculation (bounces off surfaces, dies on ball collision)
    NewX = X + VX,
    NewY = Y + VY,
    
    %% Check virus collisions (surfaces and balls)
    {FinalX, FinalY, FinalVX, FinalVY, VirusAlive} = handle_virus_collisions(NewX, NewY, VX, VY, VirusId),
    
    %% Determine which quadrant the virus should be in
    NewQuadrant = get_quadrant_from_pos({FinalX, FinalY}),
    
    %% Virus collision calculation complete
    
    %% Extra debug for boundary crossings
    if RequestingQuadrant =/= NewQuadrant ->
        ok;
    true -> ok
    end,
    
    Result = case VirusAlive of
        true ->
            {
                {FinalX, FinalY},      % New position
                {FinalVX, FinalVY},    % New velocity
                NewQuadrant            % New owner quadrant
            };
        false ->
            %% Virus died from ball collision - return special death signal
            {
                {FinalX, FinalY},      % Final position
                {0, 0},                % Stop movement
                virus_killed           % Special signal for death
            }
    end,
    
    {reply, Result, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_call}, State}.

handle_cast(start_game, State) ->
    {noreply, State};

handle_cast(_Cast, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% @private Handle all collision types (walls, paddle, bricks)
handle_all_collisions(NewX, NewY, VX, VY) ->
    %% First, keep ball within bounds to prevent teleporting
    BoundedNewX = max(10, min(790, NewX)),
    BoundedNewY = max(40, NewY),
    
    %% Check for wall collisions (direction-aware to prevent stuck balls)
    FinalVX = if
        BoundedNewX =< 10 andalso VX < 0 -> -VX;  % Left wall hit while moving left
        BoundedNewX >= 790 andalso VX > 0 -> -VX; % Right wall hit while moving right
        true -> VX
    end,
    
    %% Check for top boundary collision (direction-aware)
    {TempVX, TempVY} = if
        BoundedNewY =< 40 andalso VY < 0 -> {FinalVX, -VY};  % Top wall hit while moving up
        true -> {FinalVX, VY}
    end,
    
    %% Check for brick collision first (closest brick only, per your requirement)
    {PostBrickVX, PostBrickVY} = check_brick_collisions(BoundedNewX, BoundedNewY, TempVX, TempVY),
    
    %% Check for paddle collision (may modify both VX and VY)  
    {FinalVX2, FinalVY} = case ets:lookup(game_objects, paddle) of
        [{paddle, {PX, PY}, _, _}] ->
            PaddleHalfWidth = 50,  % PADDLE_WIDTH = 100, so half = 50
            PaddleHeight = 20,
            PaddleTop = PY - (PaddleHeight div 2),
            PaddleBottom = PY + (PaddleHeight div 2),
            if
                % Ball hits paddle from above (using bounded positions to prevent teleporting)
                BoundedNewY >= PaddleTop andalso BoundedNewY =< PaddleBottom andalso 
                BoundedNewX >= (PX - PaddleHalfWidth) andalso BoundedNewX =< (PX + PaddleHalfWidth) andalso
                PostBrickVY > 0 ->  % Only bounce if ball is moving downward toward paddle
                    
                    %% Calculate angle-based bounce physics
                    %% Hit position relative to paddle center (-1.0 to 1.0)
                    HitOffset = (BoundedNewX - PX) / PaddleHalfWidth,
                    
                    %% Current speed (preserve total velocity magnitude)
                    CurrentSpeed = math:sqrt(PostBrickVX*PostBrickVX + PostBrickVY*PostBrickVY),
                    
                    %% Calculate new velocity components based on hit position
                    %% Max bounce angle: 60 degrees (about 1.0 radians)
                    MaxAngle = 1.0,  % radians (about 57 degrees)
                    BounceAngle = HitOffset * MaxAngle,
                    
                    %% New velocity components
                    NewVX = CurrentSpeed * math:sin(BounceAngle),
                    NewVY = -CurrentSpeed * math:cos(BounceAngle),  % Always bounce upward
                    
                    {NewVX, NewVY};  % Return both VX and VY
                % Ball hits bottom of screen (miss) - use bounded position
                BoundedNewY > 580 ->
                    %% Return special "game over" velocity to signal ball loss
                    {0, -999};  % Special signal: VY=-999 means ball missed
                true -> 
                    {PostBrickVX, PostBrickVY}  % Return velocity after brick collision
            end;
        [] -> 
            {PostBrickVX, PostBrickVY}
    end,
    
    %% Return the bounded positions to prevent teleporting
    {BoundedNewX, BoundedNewY, FinalVX2, FinalVY}.

%% @private Determine which quadrant a position belongs to
get_quadrant_from_pos({X, Y}) when X < 400, Y < 300 -> q1;
get_quadrant_from_pos({X, Y}) when X >= 400, Y < 300 -> q2;
get_quadrant_from_pos({X, Y}) when X < 400, Y >= 300 -> q3;
get_quadrant_from_pos({X, Y}) when X >= 400, Y >= 300 -> q4.

%% @private Check for brick collisions and handle distributed damage
check_brick_collisions(BallX, BallY, VX, VY) ->
    %% Get all brick objects from ETS
    AllObjects = ets:tab2list(game_objects),
    BrickObjects = [Obj || Obj <- AllObjects, element(1, element(1, Obj)) == brick],
    
    %% Find closest brick that the ball would collide with
    case find_closest_brick_collision(BallX, BallY, VX, VY, BrickObjects) of
        no_collision ->
            {VX, VY};  % No collision, return original velocity
        {BrickId, Owner, CollisionSide, BrickX, BrickY} ->
            %% Send damage message to the quadrant manager that owns the brick
            
            %% Send distributed damage message
            quadrant_manager:damage_brick(Owner, BrickId),
            
            %% Update score (basic hit points)
            game_server:add_score(10),
            
            %% Check if AOE damage should be applied
            CurrentScore = game_server:get_current_score(),
            case CurrentScore >= 1000 of  %% AOE_DAMAGE_THRESHOLD
                true ->
                    %% Apply AOE damage to nearby bricks
                    apply_aoe_damage(BrickX, BrickY);
                false ->
                    ok
            end,
            
            %% Calculate bounce based on collision side
            bounce_off_brick(VX, VY, CollisionSide)
    end.

%% @private Find the closest brick that would be hit by the ball
find_closest_brick_collision(BallX, BallY, VX, VY, BrickObjects) ->
    BallRadius = 12,  % Ball radius
    BrickWidth = 75,   % From Python GUI
    BrickHeight = 25,  % From Python GUI
    
    %% Check each brick for collision
    Collisions = lists:filtermap(fun({{brick, BrickId}, {BrickX, BrickY}, _Health, Owner, _SpecialEffect}) ->
        %% Check if ball intersects with brick
        BrickLeft = BrickX - BrickWidth/2,
        BrickRight = BrickX + BrickWidth/2,
        BrickTop = BrickY - BrickHeight/2,
        BrickBottom = BrickY + BrickHeight/2,
        
        %% Simple  collision detection
        if BallX + BallRadius >= BrickLeft andalso BallX - BallRadius =< BrickRight andalso
           BallY + BallRadius >= BrickTop andalso BallY - BallRadius =< BrickBottom ->
            %% Collision detected - determine which side
            CollisionSide = determine_collision_side(BallX, BallY, VX, VY, BrickX, BrickY, BrickWidth, BrickHeight),
            Distance = math:sqrt((BallX - BrickX)*(BallX - BrickX) + (BallY - BrickY)*(BallY - BrickY)),
            {true, {Distance, BrickId, Owner, CollisionSide, BrickX, BrickY}};
        true ->
            false
        end
    end, BrickObjects),
    
    %% Return the closest collision (if any)
    case Collisions of
        [] -> no_collision;
        [_|_] -> 
            %% Sort by distance and return the closest
            [{_Distance, BrickId, Owner, CollisionSide, BrickX, BrickY}|_] = lists:sort(Collisions),
            {BrickId, Owner, CollisionSide, BrickX, BrickY}
    end.

%% @private Determine which side of the brick was hit
determine_collision_side(BallX, BallY, VX, VY, BrickX, BrickY, BrickWidth, BrickHeight) ->
    %% Calculate relative position
    RelX = BallX - BrickX,
    RelY = BallY - BrickY,
    
    %% Determine collision side based on ball velocity and relative position
    if abs(RelX) / (BrickWidth/2) > abs(RelY) / (BrickHeight/2) ->
        %% Horizontal collision
        if VX > 0 -> left; VX < 0 -> right; true -> left end;
    true ->
        %% Vertical collision  
        if VY > 0 -> top; VY < 0 -> bottom; true -> top end
    end.

%% @private Calculate bounce velocity based on collision side
bounce_off_brick(VX, VY, CollisionSide) ->
    case CollisionSide of
        left -> {-abs(VX), VY};   % Bounce right
        right -> {abs(VX), VY};   % Bounce left
        top -> {VX, -abs(VY)};    % Bounce down
        bottom -> {VX, abs(VY)}   % Bounce up
    end.

%% @private Handle bomb collisions (only paddle collision for damage)
handle_bomb_collisions(BombX, BombY, VX, VY) ->
    %% Check bomb-paddle collision for player damage
    case check_bomb_paddle_collision(BombX, BombY, VX, VY) of
        collision ->
            %% Damage player (decrement lives)
            game_server:decrement_life(),
            %% Bomb disappears after hitting paddle
            {BombX, 700, 0, 0}; % Move bomb off-screen to trigger cleanup
        no_collision ->
            %% No collision, continue falling
            {BombX, BombY, VX, VY}
    end.

%% @private Check if bomb collides with paddle
check_bomb_paddle_collision(BombX, BombY, _VX, _VY) ->
    %% Get paddle position from ETS
    case ets:lookup(game_objects, paddle) of
        [{paddle, {PaddleX, PaddleY}, _Vel, _Owner}] ->
            %% Paddle dimensions (same as ball collision)
            PaddleWidth = 80,
            PaddleHeight = 20,
            BombRadius = 5, % Assume small bomb size
            
            %% AABB collision detection
            if (BombX + BombRadius >= PaddleX) and 
               (BombX - BombRadius =< PaddleX + PaddleWidth) and
               (BombY + BombRadius >= PaddleY) and 
               (BombY - BombRadius =< PaddleY + PaddleHeight) ->
                collision;
            true ->
                no_collision
            end;
        [] ->
            %% No paddle found
            no_collision
    end.

%% @private Handle virus collisions with surfaces and balls
handle_virus_collisions(NewX, NewY, VX, VY, VirusId) ->
    %% First, check for ball collision (virus dies if hit by ball)
    case check_virus_ball_collision(NewX, NewY, VirusId) of
        ball_collision ->
            %% Virus dies - return position but mark as dead
            {NewX, NewY, 0, 0, false};
        no_ball_collision ->
            %% No ball collision, check surface bouncing
            handle_virus_surface_collisions(NewX, NewY, VX, VY)
    end.

%% @private Handle virus bouncing off surfaces (walls)
handle_virus_surface_collisions(NewX, NewY, VX, VY) ->
    %% Virus bounces off all four walls
    VirusRadius = 8,  % Virus size
    
    %% Keep virus within bounds and bounce off walls
    FinalX = max(VirusRadius, min(800 - VirusRadius, NewX)),
    FinalY = max(VirusRadius, min(600 - VirusRadius, NewY)),
    
    %% Check for wall collisions and reverse velocity
    FinalVX = if
        NewX =< VirusRadius andalso VX < 0 -> -VX;     % Left wall hit
        NewX >= (800 - VirusRadius) andalso VX > 0 -> -VX;  % Right wall hit  
        true -> VX
    end,
    
    FinalVY = if
        NewY =< VirusRadius andalso VY < 0 -> -VY;     % Top wall hit
        NewY >= (600 - VirusRadius) andalso VY > 0 -> -VY;  % Bottom wall hit
        true -> VY
    end,
    
    %% Virus stays alive after bouncing
    {FinalX, FinalY, FinalVX, FinalVY, true}.

%% @private Check if virus collides with any ball
check_virus_ball_collision(VirusX, VirusY, _VirusId) ->
    %% Get all ball objects from ETS
    AllObjects = ets:tab2list(game_objects),
    BallObjects = [Obj || Obj <- AllObjects, element(1, element(1, Obj)) == ball],
    
    VirusRadius = 8,
    BallRadius = 12,
    CollisionDistance = VirusRadius + BallRadius,
    
    %% Check each ball for collision
    case lists:any(fun({{ball, _BallId}, {BallX, BallY}, _Vel, _Owner}) ->
        Distance = math:sqrt((VirusX - BallX)*(VirusX - BallX) + (VirusY - BallY)*(VirusY - BallY)),
        Distance =< CollisionDistance
    end, BallObjects) of
        true -> ball_collision;
        false -> no_ball_collision
    end.

%% @private Apply AOE damage to adjacent bricks
apply_aoe_damage(HitBrickX, HitBrickY) ->
    
    %% Get brick dimensions from initialization
    BrickWidth = 80,   %% From brick initialization
    BrickHeight = 30,  %% From brick initialization
    
    %% Get all brick objects from ETS
    AllObjects = ets:tab2list(game_objects),
    BrickObjects = [Obj || Obj <- AllObjects, element(1, element(1, Obj)) == brick],
    
    %% Find adjacent bricks
    AdjacentBricks = lists:filtermap(fun({{brick, BrickId}, {BrickX, BrickY}, _Health, Owner, _SpecialEffect}) ->
        %% Skip the hit brick itself
        if BrickX =:= HitBrickX andalso BrickY =:= HitBrickY ->
            false;
        true ->
            %% Check horizontal adjacency (exactly touching)
            HorizontalDistance = abs(BrickX - HitBrickX),
            HorizontalAdjacent = HorizontalDistance =:= BrickWidth,
            
            %% Check vertical adjacency (exactly touching)
            VerticalDistance = abs(BrickY - HitBrickY),
            VerticalAdjacent = VerticalDistance =:= BrickHeight,
            
            %% Both must be true for adjacency
            if HorizontalAdjacent andalso VerticalAdjacent ->
                {true, {BrickId, Owner, HorizontalDistance, VerticalDistance}};
            true ->
                false
            end
        end
    end, BrickObjects),
    
    
    %% Apply damage to each adjacent brick using existing damage system
    lists:foreach(fun({BrickId, Owner, _HDist, _VDist}) ->
        %% Use the existing damage_brick function - no new code needed!
        quadrant_manager:damage_brick(Owner, BrickId),
        ok
    end, AdjacentBricks),
    
    ok.
