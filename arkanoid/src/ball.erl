-module(ball).
-behaviour(gen_statem).

%% Client API
-export([start_link/2, get_state/1, launch/3, get_position/1, get_full_state/1, update_state/3, update_paddle_pos/2]).
-export([start_link/1, get_state/0, launch/2, get_position/0, get_full_state/0, update_state/2]). %% old functions , might need  in the future

%% gen_statem callbacks
-export([init/1, callback_mode/0, terminate/3, code_change/4]).
-export([idle/3, moving/3]).

-define(SERVER, ?MODULE).
-define(WIDTH, 800).
-define(HEIGHT, 600).
-define(PADDLE_Y, ?HEIGHT - 30).
-define(PADDLE_WIDTH, 100).

%%%===================================================================
%%% Client API
%%%===================================================================

%% @doc Starts the ball process with ID.
%% BallId is an atom like ball_1, ball_2, etc.
start_link(BallId, InitialData) ->
    %% Create a unique atom name for registration (e.g., ball_1 -> ball_ball_1)
    ServerName = list_to_atom("ball_" ++ atom_to_list(BallId)),
    gen_statem:start_link({local, ServerName}, ?MODULE, [InitialData#{id => BallId}], []).


start_link(InitialData) ->
    BallId = maps:get(id, InitialData, ball_1),
    start_link(BallId, InitialData).

%% @doc Get the ball's current state atom by ID.
get_state(BallId) ->
    ServerName = list_to_atom("ball_" ++ atom_to_list(BallId)),
    gen_statem:call(ServerName, get_state).

%% @doc Launch the ball from a position with a certain velocity by ID.
launch(BallId, Pos, Vel) ->
    ServerName = list_to_atom("ball_" ++ atom_to_list(BallId)),
    gen_statem:cast(ServerName, {launch, Pos, Vel}).

%% @doc Get the ball's current position by ID.
get_position(BallId) ->
    ServerName = list_to_atom("ball_" ++ atom_to_list(BallId)),
    gen_statem:call(ServerName, get_position).

%% @doc Get the ball's full state (pos and vel) by ID.
get_full_state(BallId) ->
    ServerName = list_to_atom("ball_" ++ atom_to_list(BallId)),
    gen_statem:call(ServerName, get_full_state).

%% @doc Update the ball's position and velocity by ID.
update_state(BallId, Pos, Vel) ->
    ServerName = list_to_atom("ball_" ++ atom_to_list(BallId)),
    gen_statem:cast(ServerName, {update_state, Pos, Vel}).

get_state() ->
    gen_statem:call(?SERVER, get_state).

launch(Pos, Vel) ->
    gen_statem:cast(?SERVER, {launch, Pos, Vel}).

get_position() ->
    gen_statem:call(?SERVER, get_position).

get_full_state() ->
    gen_statem:call(?SERVER, get_full_state).

update_state(Pos, Vel) ->
    gen_statem:cast(?SERVER, {update_state, Pos, Vel}).

%% @doc Update the paddle's X position.
update_paddle_pos(BallProcess, PaddleX) ->
    gen_statem:cast(BallProcess, {update_paddle_pos, PaddleX}).



%%%===================================================================
%%% gen_statem callbacks
%%%===================================================================

callback_mode() ->
    state_functions.

init([InitialData]) ->
    %% Ball always starts in 'idle' state, waiting for launch.
    _BallId = maps:get(id, InitialData, ball_1),
    {ok, idle, InitialData#{paddle_pos => ?WIDTH / 2}}.

terminate(_Reason, _State, _Data) ->
    ok.

code_change(_Vsn, State, Data, _Extra) ->
    {ok, State, Data}.

%%%===================================================================
%%% State functions
%%%===================================================================

%% @doc The ball is waiting to be launched by the player.
idle(cast, {launch, Pos, Vel}, Data) ->
    _BallId = maps:get(id, Data, unknown),
    {next_state, moving, Data#{pos => Pos, vel => Vel}};
idle({call, From}, get_state, Data) ->
    gen_statem:reply(From, {ok, idle}),
    {keep_state, Data};
idle({call, From}, get_position, Data = #{pos := Pos}) ->
    gen_statem:reply(From, {ok, Pos}),
    {keep_state, Data};
idle({call, From}, get_full_state, Data = #{pos := Pos, vel := Vel}) ->
    gen_statem:reply(From, {ok, {Pos, Vel}}),
    {keep_state, Data};
idle(cast, {update_paddle_pos, PaddleX}, Data) ->
    _BallId = maps:get(id, Data, ball_unknown),
    {keep_state, Data#{paddle_pos => PaddleX}};
idle(_EventType, _EventContent, Data) ->
    {keep_state, Data}.


%% @doc The ball is in motion.
moving({call, From}, get_state, Data) ->
    gen_statem:reply(From, {ok, moving}),
    {keep_state, Data};
moving({call, From}, get_position, Data = #{pos := Pos}) ->
    gen_statem:reply(From, {ok, Pos}),
    {keep_state, Data};
moving({call, From}, get_full_state, Data = #{pos := Pos, vel := Vel}) ->
    gen_statem:reply(From, {ok, {Pos, Vel}}),
    {keep_state, Data};
moving(cast, {update_state, NewPos, NewVel}, Data) ->
    {keep_state, Data#{pos => NewPos, vel => NewVel}};
moving(cast, {update_paddle_pos, PaddleX}, Data) ->
    _BallId = maps:get(id, Data, ball_unknown),
    {keep_state, Data#{paddle_pos := PaddleX}};
moving(_EventType, _EventContent, Data) ->
    {keep_state, Data}.
