# Arkanoid - Distributed Erlang/OTP Game with Python GUI 🎮

A **fully-featured, fault-tolerant** distributed implementation of the classic Arkanoid game, built using Erlang/OTP with a Python pygame GUI. Features advanced distributed architecture, fault tolerance, AOE damage, and real-time multiplayer capabilities across multiple nodes.


### Screen Quadrant System

The game screen is divided into 4 quadrants, each managed by a dedicated slave node:

- **Q1 (Top-Left)**: `slave1@IP` - Manages objects in `x < 400, y < 300`
- **Q2 (Top-Right)**: `slave2@IP` - Manages objects in `x >= 400, y < 300`  
- **Q3 (Bottom-Left)**: `slave3@IP` - Manages objects in `x < 400, y >= 300`
- **Q4 (Bottom-Right)**: `slave4@IP` - Manages objects in `x >= 400, y >= 300`


### Core Objects

####  **Balls**
- **Multi-ball Support**: Multiple balls can exist simultaneously
- **Physics**: Realistic collision detection with walls, paddle, and bricks
- **Respawn System**: Automatic respawn after ball loss

####  **Paddle** 
- **Movement**: Responsive left/right controls (A/D keys)
- **Angle-based Bouncing**: Ball angle changes based on hit position

####  **Bricks**
- **Grid Layout**: 8×5 brick grid with random health (1-4 HP)
- **Color-coded Health**: Visual HP indication in Python GUI
- **Special Effects**: chance for special effects on destruction

### Special Effects

####  **Bombs**
- **Spawn from Special Bricks**: Random bomb drops from destroyed bricks
- **Gravity Physics**: Fall straight down at constant speed
- **Player Damage**: Hit paddle = lose 1 life

####  **Viruses**
- **Spawn from Special Bricks**: Random virus spawns
- **Duplication**: Viruses duplicate every 5 seconds
- **Ball Collision**: Viruses die when hit by balls
- **Population Control**: Game over if virus count exceeds limit

####  **AOE Damage System**
- **Score Threshold**: Activates at 1000+ points
- **Adjacent Damage**: Affects bricks adjecant to brick


## How to Run

### Prerequisites
- **Erlang/OTP 24** installed
- **Python 3.7** with `pygame` library
- **6 Terminal Windows** (4 slaves + 1 main + 1 python gui)


1. **Update IP Configuration**:
   ```bash
   # Edit src/distributed_config.erl with your machine's IP
   nano src/distributed_config.erl
   ```

2. **Start Slave Nodes** (Terminals 1-4):
   ```bash
   # Terminal 1
   rebar3 as slave1 shell --name slave1@YOUR_IP --setcookie arkanoid_cookie
   
   # Terminal 2  
   rebar3 as slave2 shell --name slave2@YOUR_IP --setcookie arkanoid_cookie
   
   # Terminal 3
   rebar3 as slave3 shell --name slave3@YOUR_IP --setcookie arkanoid_cookie
   
   # Terminal 4
   rebar3 as slave4 shell --name slave4@YOUR_IP --setcookie arkanoid_cookie
   ```

3. **Start Main Node** (Terminal 5):
   ```bash
   rebar3 as main shell --name main@YOUR_IP --setcookie arkanoid_cookie
   ```

   
4. **Start Python GUI**(Terminal 6)
   python3 arkanoid_gui.py
   ```

### Controls
- **left** = Move paddle left
- **right** = Move paddle right  
- **SPACE** = Launch ball (when idle)
- **B** = Spawn bomb (testing)
- **V** = Spawn virus (testing)


**all rights are reserved to etay kandil and naor mauda**