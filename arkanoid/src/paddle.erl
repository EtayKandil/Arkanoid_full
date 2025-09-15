-module(paddle).
-behaviour(gen_statem).

%% Client API
 -export([start_link/0, move_left/0, move_right/0, stop/0, get_position/0, get_state/0]).

%% gen_statem callbacks
-export([init/1, callback_mode/0, terminate/3, code_change/4]).
-export([idle/3, moving_left/3, moving_right/3]).

-define(SERVER, ?MODULE).
-define(PADDLE_SPEED, 10).
-define(WIDTH, 800).
-define(PADDLE_WIDTH, 100).
-define(MOVE_INTERVAL, 16).

%%%===================================================================
%%% Client API
%%%===================================================================

start_link() ->
    gen_statem:start_link({local, ?SERVER}, ?MODULE, [], []).

move_left() ->
    gen_statem:cast(?SERVER, move_left).

move_right() ->
    gen_statem:cast(?SERVER, move_right).

stop() ->
    gen_statem:cast(?SERVER, stop).

get_position() ->
    gen_statem:call(?SERVER, get_position).

get_state() ->
    gen_statem:call(?SERVER, get_state).

%%%===================================================================
%%% gen_statem callbacks
%%%===================================================================

callback_mode() ->
    state_functions.

init([]) ->
    InitialPos = {400, 550},
    InitialQuadrant = get_quadrant_from_pos(InitialPos),
    {ok, idle, #{x => 400, owner => InitialQuadrant}}.

terminate(_Reason, _State, _Data) ->
    ok.

code_change(_Vsn, State, Data, _Extra) ->
    {ok, State, Data}.

%%%===================================================================
%%% State functions
%%%===================================================================

%% @private
%% @doc Handling events in the idle state.
idle(cast, move_left, Data) ->
    {next_state, moving_left, Data, [{state_timeout, ?MOVE_INTERVAL, move}]};
idle(cast, move_right, Data) ->
    {next_state, moving_right, Data, [{state_timeout, ?MOVE_INTERVAL, move}]};
idle({call, From}, get_position, Data = #{x := X}) ->
    gen_statem:reply(From, {ok, X}),
    {keep_state, Data};
idle({call, From}, get_state, Data) ->
    gen_statem:reply(From, {ok, idle}),
    {keep_state, Data};
idle(_EventType, _EventContent, Data) ->
    {keep_state, Data}.

%% @private
%% @doc Handling events in the moving_left state.
moving_left(cast, stop, Data) ->
    {next_state, idle, Data};
moving_left(cast, move_right, Data) ->
    %% Change direction immediately
    {next_state, moving_right, Data, [{state_timeout, ?MOVE_INTERVAL, move}]};
moving_left({call, From}, get_position, Data = #{x := X}) ->
    gen_statem:reply(From, {ok, X}),
    {keep_state, Data};
moving_left({call, From}, get_state, Data) ->
    gen_statem:reply(From, {ok, moving_left}),
    {keep_state, Data};
moving_left(state_timeout, move, Data = #{x := X, owner := Owner}) ->
    NewX = X - ?PADDLE_SPEED,
    HalfPaddle = ?PADDLE_WIDTH div 2,
    if
        NewX - HalfPaddle < 0 ->
            FinalX = HalfPaddle,
            %% Send final position update to quadrant manager
            quadrant_manager:update_object_position(Owner, paddle, {FinalX, 550}, {0, 0}),
            {next_state, idle, Data#{x => FinalX}};
        true ->
            %% Send position update to quadrant manager
            quadrant_manager:update_object_position(Owner, paddle, {NewX, 550}, {-?PADDLE_SPEED, 0}),
            {keep_state, Data#{x => NewX}, [{state_timeout, ?MOVE_INTERVAL, move}]}
    end;
moving_left(cast, tick, Data = #{x := X, owner := Owner}) ->
    %% Check if paddle crossed quadrant boundary
    CurrentPos = {X, 550},
    CurrentQuadrant = get_quadrant_from_pos(CurrentPos),
    
    if 
        CurrentQuadrant =/= Owner ->
            %% Paddle crossed boundary!
            
            %% ATOMIC PADDLE OWNERSHIP TRANSFER to prevent movement command loss
            
            %% Use async calls (original approach)
            quadrant_manager:release_ownership(Owner, paddle, CurrentPos, {-?PADDLE_SPEED, 0}),
            
            quadrant_manager:take_ownership(CurrentQuadrant, paddle),
            
            {keep_state, Data#{owner => CurrentQuadrant}};
        true ->
            %% Stay in same quadrant, just update position
            quadrant_manager:update_object_position(Owner, paddle, CurrentPos, {-?PADDLE_SPEED, 0}),
            {keep_state, Data}
    end;
moving_left(_EventType, _EventContent, Data) ->
    {keep_state, Data}.

%% @private
%% @doc Handling events in the moving_right state.
moving_right(cast, stop, Data) ->
    {next_state, idle, Data};
moving_right(cast, move_left, Data) ->
    %% Change direction immediately
    {next_state, moving_left, Data, [{state_timeout, ?MOVE_INTERVAL, move}]};
moving_right({call, From}, get_position, Data = #{x := X}) ->
    gen_statem:reply(From, {ok, X}),
    {keep_state, Data};
moving_right({call, From}, get_state, Data) ->
    gen_statem:reply(From, {ok, moving_right}),
    {keep_state, Data};
moving_right(state_timeout, move, Data = #{x := X, owner := Owner}) ->
    NewX = X + ?PADDLE_SPEED,
    HalfPaddle = ?PADDLE_WIDTH div 2,
    if
        NewX + HalfPaddle > ?WIDTH ->
            FinalX = ?WIDTH - HalfPaddle,
            %% Send final position update to quadrant manager
            quadrant_manager:update_object_position(Owner, paddle, {FinalX, 550}, {0, 0}),
            {next_state, idle, Data#{x => FinalX}};
        true ->
            %% Send position update to quadrant manager
            quadrant_manager:update_object_position(Owner, paddle, {NewX, 550}, {?PADDLE_SPEED, 0}),
            {keep_state, Data#{x => NewX}, [{state_timeout, ?MOVE_INTERVAL, move}]}
    end;
moving_right(cast, tick, Data = #{x := X, owner := Owner}) ->
    %% Check if paddle crossed quadrant boundary
    CurrentPos = {X, 550},
    CurrentQuadrant = get_quadrant_from_pos(CurrentPos),
    
    if 
        CurrentQuadrant =/= Owner ->
            %% Paddle crossed boundary!
            
            %% ATOMIC PADDLE OWNERSHIP TRANSFER to prevent movement command loss
            
            %% Use async calls (original approach)
            quadrant_manager:release_ownership(Owner, paddle, CurrentPos, {?PADDLE_SPEED, 0}),
            
            quadrant_manager:take_ownership(CurrentQuadrant, paddle),
            
            {keep_state, Data#{owner => CurrentQuadrant}};
        true ->
            %% Stay in same quadrant, just update position
            quadrant_manager:update_object_position(Owner, paddle, CurrentPos, {?PADDLE_SPEED, 0}),
            {keep_state, Data}
    end;
moving_right(_EventType, _EventContent, Data) ->
    {keep_state, Data}.

%% @private Determines which quadrant a coordinate belongs to.
get_quadrant_from_pos({X, Y}) when X < 400, Y < 300 -> q1;
get_quadrant_from_pos({X, Y}) when X >= 400, Y < 300 -> q2;
get_quadrant_from_pos({X, Y}) when X < 400, Y >= 300 -> q3;
get_quadrant_from_pos({X, Y}) when X >= 400, Y >= 300 -> q4.
