%% leave for future work for expanding the game


-module(level_manager).
-behaviour(gen_server).

-export([start_link/0]).
-export([get_layout/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% public api
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

get_layout(LevelNumber) ->
    gen_server:call(?MODULE, {get_layout, LevelNumber}).

%% gen_server callbacks
init([]) ->
    {ok, #{}}.

handle_call({get_layout, _LevelNumber}, _From, State) ->
    %% For now, we ignore the LevelNumber and always return the same layout.
    %% This layout represents a simple 8x4 grid of bricks.
    %% The format is: {{Column, Row}, Type}
    Layout = [
        {{C, R}, regular} || C <- lists:seq(1, 8), R <- lists:seq(1, 4)
    ],
    {reply, {ok, Layout}, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_call}, State}.

handle_cast(_Cast, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.