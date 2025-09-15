-module(brick).
-behaviour(gen_statem).

%% Client API (similar to ball.erl)
-export([start_link/2, get_state/1, take_damage/1, get_position/1, get_health/1]).
-export([start_link/1, get_state/0, take_damage/0, get_position/0, get_health/0]). %% Legacy functions , might need  in the future

%% gen_statem callbacks
-export([init/1, callback_mode/0, terminate/3, code_change/4]).
-export([active/3, destroyed/3]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% Client API
%%%===================================================================

%% @doc Starts the brick process with ID.
start_link(BrickId, InitialData) ->
    ServerName = list_to_atom("brick_" ++ atom_to_list(BrickId)),
    gen_statem:start_link({local, ServerName}, ?MODULE, [InitialData#{id => BrickId}], []).

start_link(InitialData) ->
    BrickId = maps:get(id, InitialData, brick_1),
    start_link(BrickId, InitialData).

%% @doc Get the brick's current state atom by ID.
get_state(BrickId) ->
    ServerName = list_to_atom("brick_" ++ atom_to_list(BrickId)),
    gen_statem:call(ServerName, get_state).

%% @doc Make the brick take damage by ID.
take_damage(BrickId) ->
    ServerName = list_to_atom("brick_" ++ atom_to_list(BrickId)),
    gen_statem:cast(ServerName, take_damage).

%% @doc Get the brick's current position by ID.
get_position(BrickId) ->
    ServerName = list_to_atom("brick_" ++ atom_to_list(BrickId)),
    gen_statem:call(ServerName, get_position).

%% @doc Get the brick's current health by ID.
get_health(BrickId) ->
    ServerName = list_to_atom("brick_" ++ atom_to_list(BrickId)),
    gen_statem:call(ServerName, get_health).

%% Legacy API functions (for future use)
get_state() ->
    gen_statem:call(?SERVER, get_state).

take_damage() ->
    gen_statem:cast(?SERVER, take_damage).

get_position() ->
    gen_statem:call(?SERVER, get_position).

get_health() ->
    gen_statem:call(?SERVER, get_health).

%%%===================================================================
%%% gen_statem callbacks
%%%===================================================================

callback_mode() ->
    state_functions.

init([InitialData]) ->
    %% Brick starts in 'active' state
    _BrickId = maps:get(id, InitialData, brick_1),
    {ok, active, InitialData}.

terminate(_Reason, _State, _Data) ->
    ok.

code_change(_Vsn, State, Data, _Extra) ->
    {ok, State, Data}.

%%%===================================================================
%%% State functions
%%%===================================================================

%% @doc The brick is active and can take damage
active(cast, take_damage, Data = #{health := Health, id := _BrickId}) ->
    NewHealth = Health - 1,
    
    if NewHealth =< 0 ->
        %% Brick is destroyed
        {next_state, destroyed, Data#{health => 0}};
    true ->
        %% Brick still has health
        {keep_state, Data#{health => NewHealth}}
    end;

active({call, From}, get_state, _Data) ->
    gen_statem:reply(From, {ok, active}),
    keep_state_and_data;

active({call, From}, get_position, _Data = #{pos := Pos}) ->
    gen_statem:reply(From, {ok, Pos}),
    keep_state_and_data;

active({call, From}, get_health, _Data = #{health := Health}) ->
    gen_statem:reply(From, {ok, Health}),
    keep_state_and_data;

active(_EventType, _EventContent, _Data) ->
    keep_state_and_data.

%% @doc The brick has been destroyed
destroyed({call, From}, get_state, _Data) ->
    gen_statem:reply(From, {ok, destroyed}),
    keep_state_and_data;

destroyed({call, From}, get_position, _Data = #{pos := Pos}) ->
    gen_statem:reply(From, {ok, Pos}),
    keep_state_and_data;

destroyed({call, From}, get_health, _Data) ->
    gen_statem:reply(From, {ok, 0}),
    keep_state_and_data;

destroyed(cast, take_damage, _Data = #{id := _BrickId}) ->
    keep_state_and_data;

destroyed(_EventType, _EventContent, _Data) ->
    keep_state_and_data.

