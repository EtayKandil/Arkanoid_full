-module(slave_sup).
-behaviour(supervisor).

%% API
-export([start_link/0, start_quadrant_manager/1]).
-export([start_ball/1, stop_ball/0]).
-export([start_ball_with_id/2, stop_ball_by_id/1]). %% Multi-ball support
-export([start_brick_with_id/2, stop_brick_by_id/1]). %% Brick process support
-export([start_bomb_with_id/2, stop_bomb_by_id/1]). %% Bomb process support
-export([start_virus_with_id/2, stop_virus_by_id/1]). %% Virus process support
-export([start_paddle/0, stop_paddle/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
    Result = supervisor:start_link({local, ?SERVER}, ?MODULE, []),
    Result.

%% @doc Start a quadrant manager under this supervisor
start_quadrant_manager(Quadrant) ->
    ManagerId = list_to_atom("quadrant_manager_" ++ atom_to_list(Quadrant)),
    QuadrantManagerSpec = #{
        id => ManagerId,
        start => {quadrant_manager, start_link, [Quadrant]},
        restart => permanent,
        shutdown => 2000,
        type => worker,
        modules => [quadrant_manager]
    },
    Result = supervisor:start_child(?SERVER, QuadrantManagerSpec),
    Result.

%% @doc Start a ball process under this supervisor
start_ball(InitialData) ->
    BallSpec = #{
        id => ball,
        start => {ball, start_link, [InitialData]},
        restart => permanent,
        shutdown => 2000,
        type => worker,
        modules => [ball]
    },
    supervisor:start_child(?SERVER, BallSpec).

%% @doc Stop the ball process under this supervisor
stop_ball() ->
    % First, stop the process. Then, remove its specification so it's not restarted.
    case supervisor:terminate_child(?SERVER, ball) of
        ok -> supervisor:delete_child(?SERVER, ball);
        {error, not_found} -> ok % It's already gone, which is fine.
    end.

%% @doc Start a ball process with specific ID under this supervisor
start_ball_with_id(BallId, InitialData) ->
    %% Create a unique atom ID for the supervisor (e.g., ball_1 -> ball_ball_1)
    SupervisorId = list_to_atom("ball_" ++ atom_to_list(BallId)),
    BallSpec = #{
        id => SupervisorId,
        start => {ball, start_link, [BallId, InitialData]},
        restart => permanent,
        shutdown => 2000,
        type => worker,
        modules => [ball]
    },
    supervisor:start_child(?SERVER, BallSpec).

%% @doc Stop a specific ball process by ID under this supervisor
stop_ball_by_id(BallId) ->
    %% Create the same unique atom ID used when starting
    SupervisorId = list_to_atom("ball_" ++ atom_to_list(BallId)),
    % First, stop the process. Then, remove its specification so it's not restarted.
    case supervisor:terminate_child(?SERVER, SupervisorId) of
        ok -> supervisor:delete_child(?SERVER, SupervisorId);
        {error, not_found} -> ok % It's already gone, which is fine.
    end.

%% @doc Start a brick process with specific ID under this supervisor
start_brick_with_id(BrickId, InitialData) ->
    %% Create a unique atom ID for the supervisor (e.g., brick_1 -> brick_brick_1)
    SupervisorId = list_to_atom("brick_" ++ atom_to_list(BrickId)),
    BrickSpec = #{
        id => SupervisorId,
        start => {brick, start_link, [BrickId, InitialData]},
        restart => permanent,
        shutdown => 2000,
        type => worker,
        modules => [brick]
    },
    supervisor:start_child(?SERVER, BrickSpec).

%% @doc Stop a specific brick process by ID under this supervisor
stop_brick_by_id(BrickId) ->
    %% Create the same unique atom ID used when starting
    SupervisorId = list_to_atom("brick_" ++ atom_to_list(BrickId)),
    % First, stop the process. Then, remove its specification so it's not restarted.
    case supervisor:terminate_child(?SERVER, SupervisorId) of
        ok -> supervisor:delete_child(?SERVER, SupervisorId);
        {error, not_found} -> ok % It's already gone, which is fine.
    end.

%% @doc Start a bomb process with specific ID under this supervisor
start_bomb_with_id(BombId, InitialData) ->
    %% Create a unique atom ID for the supervisor (e.g., bomb_1 -> bomb_bomb_1)
    SupervisorId = list_to_atom("bomb_" ++ atom_to_list(BombId)),
    BombSpec = #{
        id => SupervisorId,
        start => {bomb, start_link, [BombId, InitialData]},
        restart => permanent,
        shutdown => 2000,
        type => worker,
        modules => [bomb]
    },
    supervisor:start_child(?SERVER, BombSpec).

%% @doc Stop a specific bomb process by ID under this supervisor
stop_bomb_by_id(BombId) ->
    %% Create the same unique atom ID used when starting
    SupervisorId = list_to_atom("bomb_" ++ atom_to_list(BombId)),
    % First, stop the process. Then, remove its specification so it's not restarted.
    case supervisor:terminate_child(?SERVER, SupervisorId) of
        ok -> supervisor:delete_child(?SERVER, SupervisorId);
        {error, not_found} -> ok % It's already gone, which is fine.
    end.

%% @doc Start a specific virus process by ID under this supervisor
start_virus_with_id(VirusId, InitialData) ->
    %% Create a unique atom ID for the supervisor (e.g., virus_1 -> virus_virus_1)
    SupervisorId = list_to_atom("virus_" ++ atom_to_list(VirusId)),
    VirusSpec = #{
        id => SupervisorId,
        start => {virus, start_link, [VirusId, InitialData]},
        restart => permanent,
        shutdown => 2000,
        type => worker,
        modules => [virus]
    },
    supervisor:start_child(?SERVER, VirusSpec).

%% @doc Stop a specific virus process by ID under this supervisor
stop_virus_by_id(VirusId) ->
    %% Create the same unique atom ID used when starting
    SupervisorId = list_to_atom("virus_" ++ atom_to_list(VirusId)),
    % First, stop the process. Then, remove its specification so it's not restarted.
    case supervisor:terminate_child(?SERVER, SupervisorId) of
        ok -> supervisor:delete_child(?SERVER, SupervisorId);
        {error, not_found} -> ok % It's already gone, which is fine.
    end.

%% @doc Start the paddle process under this supervisor
start_paddle() ->
    PaddleSpec = #{
        id => paddle,
        start => {paddle, start_link, []},
        restart => permanent,
        shutdown => 2000,
        type => worker,
        modules => [paddle]
    },
    supervisor:start_child(?SERVER, PaddleSpec).

%% @doc Stop the paddle process under this supervisor
stop_paddle() ->
    % First, stop the process. Then, remove its specification so it's not restarted.
    case supervisor:terminate_child(?SERVER, paddle) of
        ok -> supervisor:delete_child(?SERVER, paddle);
        {error, not_found} -> ok % It's already gone, which is fine.
    end.

%%====================================================================
%% Supervisor callbacks
%%====================================================================

init([]) ->
    SupFlags = #{
        strategy => one_for_one,
        intensity => 1,
        period => 5
    },
    
    %% Start with no children - quadrant managers will be added dynamically
    Children = [],
    
    {ok, {SupFlags, Children}}.
