
-module(arkanoid_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    case string:prefix(atom_to_list(node()), "main@") of
        nomatch ->
            Result = slave_sup:start_link(),
            Result;
        _ ->
            Result = arkanoid_sup:start_link(),
            Result
    end.

stop(_State) ->
    ok.

%% internal functions
