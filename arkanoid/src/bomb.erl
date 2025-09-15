-module(bomb).
-behaviour(gen_statem).

%% Client API (similar to ball.erl)
-export([start_link/2, get_state/1, get_position/1, get_velocity/1]).
-export([start_link/1, get_state/0, get_position/0, get_velocity/0]). %% Legacy support

%% gen_statem callbacks
-export([init/1, callback_mode/0, terminate/3, code_change/4]).
-export([falling/3, exploded/3]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% Client API
%%%===================================================================

%% @doc Starts the bomb process with ID.
%% BombId is an atom like bomb_1, bomb_2, etc.
%% InitialData should be a map like #{pos => {X, Y}, vel => {VX, VY}, id => BombId}
start_link(BombId, InitialData) ->
    %% Create a unique atom name for registration (e.g., bomb_1 -> bomb_bomb_1)
    ServerName = list_to_atom("bomb_" ++ atom_to_list(BombId)),
    gen_statem:start_link({local, ServerName}, ?MODULE, [InitialData#{id => BombId}], []).

%% @doc Legacy start_link for backward compatibility
start_link(InitialData) ->
    BombId = maps:get(id, InitialData, bomb_1),
    start_link(BombId, InitialData).

%% @doc Get the bomb's current state atom by ID.
get_state(BombId) ->
    ServerName = list_to_atom("bomb_" ++ atom_to_list(BombId)),
    gen_statem:call(ServerName, get_state).

%% @doc Get the bomb's current position by ID.
get_position(BombId) ->
    ServerName = list_to_atom("bomb_" ++ atom_to_list(BombId)),
    gen_statem:call(ServerName, get_position).

%% @doc Get the bomb's current velocity by ID.
get_velocity(BombId) ->
    ServerName = list_to_atom("bomb_" ++ atom_to_list(BombId)),
    gen_statem:call(ServerName, get_velocity).

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
    %% Bomb starts in 'falling' state with downward velocity
    _BombId = maps:get(id, InitialData, bomb_1),
    Position = maps:get(pos, InitialData, {400, 100}),
    Velocity = maps:get(vel, InitialData, {0, 3}), % Fall downward at speed 3
    
    StateData = InitialData#{
        pos => Position,
        vel => Velocity,
        id => _BombId
    },
    
    {ok, falling, StateData}.

terminate(_Reason, _State, _Data) ->
    ok.

code_change(_Vsn, State, Data, _Extra) ->
    {ok, State, Data}.

%%%===================================================================
%%% State functions
%%%===================================================================

%% @doc The bomb is falling downward
falling({call, From}, get_state, _Data) ->
    gen_statem:reply(From, {ok, falling}),
    keep_state_and_data;

falling({call, From}, get_position, _Data = #{pos := Pos}) ->
    gen_statem:reply(From, {ok, Pos}),
    keep_state_and_data;

falling({call, From}, get_velocity, _Data = #{vel := Vel}) ->
    gen_statem:reply(From, {ok, Vel}),
    keep_state_and_data;

falling(cast, update_physics, Data = #{pos := {X, Y}, vel := {VX, VY}, id := _BombId}) ->
    %% Simple falling physics - bombs just fall straight down
    NewX = X + VX,
    NewY = Y + VY,
    
    %% Check if bomb hit bottom (exploded/missed)
    if NewY > 600 ->
        {next_state, exploded, Data#{pos => {NewX, NewY}}};
    true ->
        %% Continue falling
        NewData = Data#{pos => {NewX, NewY}},
        {keep_state, NewData}
    end;

falling(_EventType, _EventContent, _Data) ->
    keep_state_and_data.

%% @doc The bomb has exploded (inactive)
exploded({call, From}, get_state, _Data) ->
    gen_statem:reply(From, {ok, exploded}),
    keep_state_and_data;

exploded({call, From}, get_position, _Data = #{pos := Pos}) ->
    gen_statem:reply(From, {ok, Pos}),
    keep_state_and_data;

exploded({call, From}, get_velocity, _Data) ->
    gen_statem:reply(From, {ok, {0, 0}}), % No velocity when exploded
    keep_state_and_data;

exploded(_EventType, _EventContent, _Data) ->
    keep_state_and_data.
