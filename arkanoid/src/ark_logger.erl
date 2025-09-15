%%leave here for future work

-module(ark_logger).
-behaviour(gen_server).
-export([start_link/0]).
-export([info/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% public api
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% Log an informational message. This is an async "fire-and-forget" call.
info(Message) ->
    gen_server:cast(?MODULE, {info, Message}).

%% gen_server callbacks
init([]) ->
    {ok, #{}}.

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_call}, State}.

handle_cast({info, Message}, State) ->
    io:format("~p [INFO] ~s~n", [self(), Message]),
    {noreply, State};

handle_cast(_Cast, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.