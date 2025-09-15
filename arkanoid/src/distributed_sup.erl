-module(distributed_sup).
-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
    Result = supervisor:start_link({local, ?SERVER}, ?MODULE, []),
    Result.

%%====================================================================
%% Supervisor callbacks
%%====================================================================

init([]) ->
    SupFlags = #{
        strategy => one_for_one,
        intensity => 3,  % Allow more restarts for network issues
        period => 10
    },
    
    %% Get node IPs from hardcoded config
    IPs = distributed_config:get_node_ips(),
    
    %% Build expected slave nodes from config
    SlaveNodes = [
        list_to_atom("slave1@" ++ maps:get(slave1, IPs)),
        list_to_atom("slave2@" ++ maps:get(slave2, IPs)),
        list_to_atom("slave3@" ++ maps:get(slave3, IPs)),
        list_to_atom("slave4@" ++ maps:get(slave4, IPs))
    ],
    
    %% Show which slaves are reachable
    lists:foreach(fun(SlaveNode) ->
        case net_adm:ping(SlaveNode) of
            pong ->
                ok;
            pang ->
                ok
        end
    end, SlaveNodes),
    
    %% Create quadrant specs using hardcoded IPs
    [Slave1Node, Slave2Node, Slave3Node, Slave4Node] = SlaveNodes,
    
    QuadrantSpecs = [
        #{
            id => quadrant_manager_q1,
            start => {quadrant_manager, start_link_remote, [q1, Slave1Node]},
            restart => permanent,
            shutdown => 5000,
            type => worker,
            modules => [quadrant_manager]
        },
        #{
            id => quadrant_manager_q2,
            start => {quadrant_manager, start_link_remote, [q2, Slave2Node]},
            restart => permanent,
            shutdown => 5000,
            type => worker,
            modules => [quadrant_manager]
        },
        #{
            id => quadrant_manager_q3,
            start => {quadrant_manager, start_link_remote, [q3, Slave3Node]},
            restart => permanent,
            shutdown => 5000,
            type => worker,
            modules => [quadrant_manager]
        },
        #{
            id => quadrant_manager_q4,
            start => {quadrant_manager, start_link_remote, [q4, Slave4Node]},
            restart => permanent,
            shutdown => 5000,
            type => worker,
            modules => [quadrant_manager]
        }
    ],
    
    {ok, {SupFlags, QuadrantSpecs}}.
