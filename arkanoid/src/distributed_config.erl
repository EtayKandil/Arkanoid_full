-module(distributed_config).
-export([get_node_ips/0, get_node_from_quadrant/1]).

%% @doc SIMPLE HARDCODED CONFIG - Update these IPs when moving to new PCs

get_node_ips() ->
    
    
    %% UPDATE THESE IPs WHEN RUNNING ON NEW COMPUTERSffffff:
    NodeIPs = #{
        main => "172.17.76.13",    % PC1 - Main node
        slave1 => "172.17.76.13",  % PC2 - Slave 1 (Quadrant q1)
        slave2 => "172.17.76.13",  % PC3 - Slave 2 (Quadrant q2) 
        slave3 => "172.17.76.13",  % PC4 - Slave 3 (Quadrant q3)
        slave4 => "172.17.76.13"   % PC5 - Slave 4 (Quadrant q4)
    },
    
    
    NodeIPs.

%% @doc Returns the full node name for a given quadrant
get_node_from_quadrant(q1) ->
    IPs = get_node_ips(),
    IP = maps:get(slave1, IPs),
    list_to_atom("slave1@" ++ IP);
get_node_from_quadrant(q2) ->
    IPs = get_node_ips(),
    IP = maps:get(slave2, IPs),
    list_to_atom("slave2@" ++ IP);
get_node_from_quadrant(q3) ->
    IPs = get_node_ips(),
    IP = maps:get(slave3, IPs),
    list_to_atom("slave3@" ++ IP);
get_node_from_quadrant(q4) ->
    IPs = get_node_ips(),
    IP = maps:get(slave4, IPs),
    list_to_atom("slave4@" ++ IP).
