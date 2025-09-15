-module(fallback_sup).
-behaviour(supervisor).

-export([start_link/1]).
-export([init/1]).

start_link(Quadrant) ->
    %% Create unique supervisor name for each quadrant
    SupervisorName = list_to_atom("fallback_sup_" ++ atom_to_list(Quadrant)),
    Result = supervisor:start_link({local, SupervisorName}, ?MODULE, [Quadrant]),
    Result.

init([Quadrant]) ->
    SupFlags = #{strategy => one_for_one, intensity => 1, period => 5},

    %% Dynamically create an atom like 'quadrant_manager_q1' for the ID
    ManagerId = list_to_atom("quadrant_manager_" ++ atom_to_list(Quadrant)),

    %% quadrant_manager: Manages a specific quadrant of the game area.
    QuadrantManagerSpec = #{
        id => ManagerId,
        start => {quadrant_manager, start_link, [Quadrant]},
        restart => permanent,
        shutdown => 2000,
        type => worker,
        modules => [quadrant_manager]
    },

    Children = [QuadrantManagerSpec],

    {ok, {SupFlags, Children}}.