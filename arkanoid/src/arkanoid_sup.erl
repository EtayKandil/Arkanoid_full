-module(arkanoid_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

start_link() ->
    Result = supervisor:start_link({local, ?MODULE}, ?MODULE, []),
    Result.

init([]) ->
    SupFlags = #{strategy => one_for_one, intensity => 1, period => 5},

    %% game_server: Manages global state like score, lives, etc.
    GameServerSpec = #{
        id => game_server,
        start => {game_server, start_link, []},
        restart => permanent,
        shutdown => 2000,
        type => worker,
        modules => [game_server]
    },


    %% paddle: Paddle state machine (gen_statem)
    PaddleSpec = #{
        id => paddle,
        start => {paddle, start_link, []},
        restart => permanent,
        shutdown => 2000,
        type => worker,
        modules => [paddle]
    },

    %% collision_manager: Central physics and collision detection engine
    CollisionManagerSpec = #{
        id => collision_manager,
        start => {collision_manager, start_link, []},
        restart => permanent,
        shutdown => 2000,
        type => worker,
        modules => [collision_manager]
    },

    %% gui: The graphical user interface.
    GuiSpec = #{
        id => gui,
        start => {gui, start_link, []},
        restart => permanent,
        shutdown => 2000,
        type => worker,
        modules => [gui]
    },

    %% distributed_sup: Supervises remote quadrant managers from main node
    DistributedSupSpec = #{
        id => distributed_sup,
        start => {distributed_sup, start_link, []},
        restart => permanent,
        shutdown => 5000,
        type => supervisor,
        modules => [distributed_sup]
    },

    %% fault_recovery_manager: Handles slave node failures and recovery
    FaultRecoverySpec = #{
        id => fault_recovery_manager,
        start => {fault_recovery_manager, start_link, []},
        restart => permanent,
        shutdown => 2000,
        type => worker,
        modules => [fault_recovery_manager]
    },

    Children = [
        GameServerSpec,
        PaddleSpec,
        CollisionManagerSpec,
        GuiSpec,
        DistributedSupSpec,
        FaultRecoverySpec
    ],

    {ok, {SupFlags, Children}}.
