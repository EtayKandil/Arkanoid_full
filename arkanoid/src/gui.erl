-module(gui).
-behaviour(gen_server).

-export([start_link/0, game_over/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(RENDER_INTERVAL, 16).
-define(GUI_PORT, 8001). %% This should probably come from a config file - check with naor

-record(state, {}).

%% public api
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

game_over() ->
    gen_server:cast(?MODULE, game_over).

%% gen_server callbacks
init([]) ->
    socket_server:start_link(?GUI_PORT),
    erlang:send_after(?RENDER_INTERVAL, self(), render),
    {ok, #state{}}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(game_over, State) ->
    %% Here we will need to send a game_over message via the socket
    {noreply, State};
handle_cast(_Cast, State) ->
    {noreply, State}.

handle_info(render, State) ->
    %% Fetch the game state and send it to the socket server to be broadcast
    GameState = game_server:get_state(),
    socket_server:send_update(GameState),
    erlang:send_after(?RENDER_INTERVAL, self(), render),
    {noreply, State};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.