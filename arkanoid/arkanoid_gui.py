#!/usr/bin/env python3
"""
Arkanoid GUI - Simple Test Interface with Pygame
Connects to Erlang Arkanoid server and provides basic game controls.
"""

import pygame
import sys
import socket
import json
import struct
import threading
import math
import time
from typing import Optional, Dict, Any

# Initialize Pygame
pygame.init()

# Server configuration
SERVER_HOST = 'localhost'
SERVER_PORT = 8001  # Should match the GUI_PORT in gui.erl
SOCKET_TIMEOUT = 5.0

# UI Configuration
WINDOW_WIDTH = 800
WINDOW_HEIGHT = 650
FPS = 60

# Colors
COLORS = {
    'BACKGROUND': (25, 35, 45),
    'UI_BACKGROUND': (35, 45, 55),
    'BUTTON_NORMAL': (70, 130, 180),
    'BUTTON_HOVER': (100, 149, 237),
    'BUTTON_DISABLED': (105, 105, 105),
    'BUTTON_CONNECT': (46, 204, 113),  # Green
    'BUTTON_START': (241, 196, 15),   # Yellow
    'TEXT_WHITE': (255, 255, 255),
    'TEXT_GREEN': (0, 255, 0),
    'TEXT_RED': (255, 0, 0),
    'TEXT_YELLOW': (255, 255, 0),
    'BORDER': (200, 200, 200),
    # Brick colors based on HP
    'BRICK_FULL_HP': (255, 100, 100),    # Red - Full health
    'BRICK_HIGH_HP': (255, 150, 50),     # Orange - High health
    'BRICK_MED_HP': (255, 255, 100),     # Yellow - Medium health
    'BRICK_LOW_HP': (100, 255, 100),     # Green - Low health
    'BRICK_CRITICAL': (100, 100, 255),   # Blue - Critical health
}

# Muted white for highlights / text that should not be pure white
DARK_WHITE = (230, 230, 235)
def get_brick_color_by_hp(brick_data):
    """
    Determine brick color based on HP level.
    Returns RGB tuple for brick color.
    """
    # Get HP from brick data, default to 3 if not specified
    hp = brick_data.get('hp', brick_data.get('health', 3))
    
    # Color based on HP value (simple integer comparison)
    if hp >= 3:
        return COLORS['BRICK_FULL_HP']      # Red - Full health (3+ HP)
    elif hp == 2:
        return COLORS['BRICK_HIGH_HP']      # Orange - High health (2 HP)
    elif hp == 1:
        return COLORS['BRICK_MED_HP']       # Yellow - Medium health (1 HP)
    else:
        return COLORS['BRICK_CRITICAL']     # Blue - Critical/Destroyed (0 HP)

class Button:
    def __init__(self, x, y, width, height, text, callback=None, color_key=None):
        self.rect = pygame.Rect(x, y, width, height)
        self.text = text
        self.callback = callback
        self.enabled = True
        self.hovered = False
        self.color_key = color_key  # Optional: custom color for this button

    def draw(self, surface, font):
        # Choose color based on state and color_key
        if not self.enabled:
            color = COLORS['BUTTON_DISABLED']
        elif self.hovered:
            color = COLORS['BUTTON_HOVER']
        elif self.color_key and self.color_key in COLORS:
            color = COLORS[self.color_key]
        else:
            color = COLORS['BUTTON_NORMAL']

        # Draw button
        pygame.draw.rect(surface, color, self.rect)
        pygame.draw.rect(surface, COLORS['BORDER'], self.rect, 2)

        # Draw text
        text_color = COLORS['TEXT_WHITE']
        text_surface = font.render(self.text, True, text_color)
        text_rect = text_surface.get_rect(center=self.rect.center)
        surface.blit(text_surface, text_rect)
    
    def handle_event(self, event):
        if event.type == pygame.MOUSEMOTION:
            self.hovered = self.rect.collidepoint(event.pos)
        elif event.type == pygame.MOUSEBUTTONDOWN:
            if event.button == 1 and self.rect.collidepoint(event.pos) and self.enabled:
                if self.callback:
                    self.callback()
                return True
        return False

class ArkanoidGUI:
    def __init__(self):
        # Pygame setup
        self.screen = pygame.display.set_mode((WINDOW_WIDTH, WINDOW_HEIGHT))
        pygame.display.set_caption("🎮 Arkanoid Game Controller")
        self.clock = pygame.time.Clock()
        
        # Fonts
        self.title_font = pygame.font.Font(None, 36)
        self.font = pygame.font.Font(None, 24)
        self.small_font = pygame.font.Font(None, 20)
        
        # Connection state
        self.socket: Optional[socket.socket] = None
        self.connected = False
        self.receive_thread: Optional[threading.Thread] = None
        self.running = True
        
        # Game state
        self.game_started = False
        self.game_status = "Game not started"
        self.connection_status = "Disconnected"
        # Numeric HUD state
        self.score = 0
        self.lives = 3
        self.level = 1
        # Track disqualifications and game over state (init to safe defaults)
        self.disqualifications = 0
        self.game_over = False
        
        # Log messages
        self.log_messages = []
        self.max_log_messages = 15
        
        # Game objects for rendering
        self.game_objects = {
            'balls': [],  # Changed to list to support multiple balls
            'paddle': None,
            'bricks': [],
            'bombs': [],
            'viruses': [],
        }
        
        # Game window
        self.game_window = None
        self.game_screen = None
        
        # Setup UI
        self.setup_buttons()
        
        # Auto-connect attempt
        self.auto_connect_time = time.time() + 1.0  # Connect after 1 second
        # Paddle visual state for improved appearance (Option A)
        self.paddle_last_x = None
        self.paddle_velocity = 0.0
        self.paddle_hit_flash = 0.0  # frames of hit flash remaining
        # Cache for procedurally drawn paddle surfaces keyed by (width, height, color_key)
        self._paddle_surface_cache = {}
        # Track previous per-ball collision state to detect new hits
        self._ball_prev_collisions = set()
        # Network suspended flag: when True we stop sending/receiving (set on GAME OVER)
        self.network_suspended = False
        # (removed per-ball color override — color no longer changes on hit)
        
    def setup_buttons(self):
        """Setup UI buttons"""
        button_width = 200
        button_height = 40
        button_gap = 20
        total_height = 2 * button_height + button_gap
        start_y = (WINDOW_HEIGHT - total_height) // 2
        button_x = (WINDOW_WIDTH - button_width) // 2

        self.buttons = {
            'connect': Button(button_x, start_y, button_width, button_height,
                             "Connect to Server", self.toggle_connection, color_key='BUTTON_CONNECT'),
            'start_game': Button(button_x, start_y + button_height + button_gap, button_width, button_height,
                                " START GAME", self.start_game, color_key='BUTTON_START')
        }
        
        # Initially disable game buttons
        self.buttons['start_game'].enabled = False
    
    def log(self, message: str):
        """Add message to log"""
        timestamp = time.strftime("%H:%M:%S")
        log_message = f"[{timestamp}] {message}"
        self.log_messages.append(log_message)
        
        # Keep only recent messages
        if len(self.log_messages) > self.max_log_messages:
            self.log_messages = self.log_messages[-self.max_log_messages:]
        
        print(log_message)  # Also print to console
    
    def toggle_connection(self):
        """Toggle connection to server"""
        if self.connected:
            self.disconnect()
        else:
            self.connect()
    
    def connect(self):
        """Connect to Erlang server"""
        # Prevent reconnecting if network has been suspended due to GAME OVER
        if getattr(self, 'network_suspended', False):
            self.log("⚠️ Network suspended: not connecting after GAME OVER")
            return
        try:
            self.log(f"Connecting to {SERVER_HOST}:{SERVER_PORT}...")
            
            self.socket = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
            self.socket.settimeout(SOCKET_TIMEOUT)
            
            # Enable keepalive
            self.socket.setsockopt(socket.SOL_SOCKET, socket.SO_KEEPALIVE, 1)
            
            self.socket.connect((SERVER_HOST, SERVER_PORT))
            self.connected = True
            
            # Update UI
            self.connection_status = "Connected"
            self.buttons['connect'].text = "Disconnect"
            self.buttons['start_game'].enabled = True
            
            # Start receive thread
            self.receive_thread = threading.Thread(target=self.receive_messages, daemon=True)
            self.receive_thread.start()
            
            self.log("✅ Connected successfully!")
            
        except Exception as e:
            self.log(f"❌ Connection failed: {e}")
            self.connected = False
            self.connection_status = "Connection Failed"
    
    def disconnect(self):
        """Disconnect from server"""
        self.connected = False
        
        if self.socket:
            self.socket.close()
            self.socket = None
        
        # Update UI
        self.connection_status = "Disconnected"
        self.buttons['connect'].text = "Connect to Server"
        self.buttons['start_game'].enabled = False
        self.game_status = "Game not started"
        
        # Close game window if open
        if self.game_window:
            self.close_game_window()
        
        self.log("🔌 Disconnected")
    
    def receive_messages(self):
        """Receive messages from server in background thread"""
        buffer = b''
        
        while self.connected:
            # If network suspended (e.g. after GAME OVER), stop receiving
            if getattr(self, 'network_suspended', False):
                self.log("🛑 Network suspended: stopping receive loop")
                break
            try:
                if not self.socket:
                    break
                    
                data = self.socket.recv(4096)
                if not data:
                    self.log("Server disconnected")
                    break
                
                # self.log(f"🔍 RAW DATA RECEIVED: {data[:100]}..." if len(data) > 100 else f"🔍 RAW DATA RECEIVED: {data}")
                buffer += data
                
                # Try to parse as direct JSON first (for test messages)
                try:
                    message = json.loads(buffer.decode('utf-8'))
                    self.log(f"📨 DIRECT JSON: {message}")
                    self.handle_server_message(message)
                    buffer = b''
                    continue
                except (UnicodeDecodeError, json.JSONDecodeError):
                    pass
                
                # Process length-prefixed packets
                while len(buffer) >= 4:
                    # Read packet length (big-endian 32-bit)
                    packet_length = struct.unpack('>I', buffer[:4])[0]
                    # self.log(f"📏 Expected packet length: {packet_length}")
                    
                    if len(buffer) >= 4 + packet_length:
                        # Complete packet available
                        packet_data = buffer[4:4 + packet_length]
                        buffer = buffer[4 + packet_length:]
                        
                        # Parse JSON message
                        try:
                            message = json.loads(packet_data.decode('utf-8'))
                            # self.log(f"📨 PARSED MESSAGE: {message}")
                            self.handle_server_message(message)
                        except (UnicodeDecodeError, json.JSONDecodeError) as e:
                            self.log(f"Failed to parse message: {e}")
                    else:
                        # Wait for more data
                        break
                        
            except socket.timeout:
                continue
            except Exception as e:
                if self.connected:
                    self.log(f"Receive error: {e}")
                break
        
        # Disconnect if we exit the loop
        if self.connected:
            self.disconnect()
    
    def handle_server_message(self, message: Dict[str, Any]):
        """Handle message received from server"""
        msg_type = message.get('type', 'unknown')
        
        if msg_type == 'update_state':
            # Game state update
            payload = message.get('payload', {})
            # Extract game objects early
            objects = payload.get('objects', [])

            score = payload.get('score', 0)
            lives = payload.get('lives', 3)
            level = payload.get('level', 1)

            # Update numeric HUD state
            prev_lives = getattr(self, 'lives', None)
            self.score = score
            self.lives = lives
            self.level = level

            # If lives decreased, count as a disqualification
            if prev_lives is not None and self.lives < prev_lives:
                self.disqualifications = getattr(self, 'disqualifications', 0) + 1
                self.log(f"⚠️ Disqualification #{self.disqualifications}")

            # Check virus count threshold from objects
            virus_count = len([o for o in objects if o.get('type') == 'virus'])

            # Update game over state if thresholds reached
            if getattr(self, 'disqualifications', 0) >= 3 or virus_count >= 5:
                # Mark game over and suspend network I/O
                self.game_over = True
                try:
                    # Avoid repeatedly suspending
                    if not getattr(self, 'network_suspended', False):
                        self.log("🔴 GAME OVER detected — suspending network I/O")
                        # Suspend network without closing the game window
                        self.network_suspended = True
                        # Close socket to stop incoming messages and mark as disconnected
                        try:
                            if self.socket:
                                try:
                                    self.socket.shutdown(socket.SHUT_RDWR)
                                except Exception:
                                    pass
                                self.socket.close()
                        except Exception:
                            pass
                        self.socket = None
                        self.connected = False
                        self.connection_status = "Suspended (GAME OVER)"
                        # Disable connect/start buttons to avoid reconnection while suspended
                        try:
                            if 'connect' in self.buttons:
                                self.buttons['connect'].enabled = False
                            if 'start_game' in self.buttons:
                                self.buttons['start_game'].enabled = False
                        except Exception:
                            pass
                except Exception as e:
                    self.log(f"Error while suspending network: {e}")

            self.game_status = f"Score: {self.score} | Lives: {self.lives} | Level: {self.level}"

            # Reset and repopulate game_objects safely
            self.game_objects['balls'] = []
            self.game_objects['paddle'] = None
            self.game_objects['bricks'] = []
            self.game_objects['bombs'] = []
            self.game_objects['viruses'] = []

            for obj in objects:
                obj_type = obj.get('type', '')
                if obj_type == 'ball':
                    self.game_objects['balls'].append(obj)
                elif obj_type == 'paddle':
                    self.game_objects['paddle'] = obj
                elif obj_type == 'brick':
                    self.game_objects['bricks'].append(obj)
                elif obj_type == 'bomb':
                    self.game_objects['bombs'].append(obj)
                elif obj_type == 'virus':
                    self.game_objects['viruses'].append(obj)
            
        else:
            self.log(f"📨 Received: {msg_type}")
    
    def create_game_window(self):
        """Create a separate window for the game"""
        if self.game_window is None:
            # Create game window
            game_width, game_height = 800, 600
            self.game_screen = pygame.display.set_mode((game_width, game_height))
            pygame.display.set_caption("🎮 Arkanoid Game")
            self.game_window = True
            self.log("🖼️ Game window opened!")
    
    def close_game_window(self):
        """Close the game window"""
        if self.game_window:
            self.game_window = None
            self.game_screen = None
            # Restore controller window
            self.screen = pygame.display.set_mode((WINDOW_WIDTH, WINDOW_HEIGHT))
            pygame.display.set_caption("🎮 Arkanoid Game Controller")
            self.log("🖼️ Game window closed!")
    
    
    def send_message(self, message: Dict[str, Any]) -> bool:
        """Send JSON message to server"""
        # Do not send any messages if network activities are suspended (GAME OVER)
        if getattr(self, 'network_suspended', False):
            self.log("⚠️ Network suspended: not sending message after GAME OVER")
            return False
        if not self.connected or not self.socket:
            self.log("⚠️ Cannot send: not connected")
            return False
            
        try:
            json_data = json.dumps(message, ensure_ascii=False)
            json_bytes = json_data.encode('utf-8')
            
            # Send length prefix + data (matching Erlang packet format)
            length_prefix = struct.pack('>I', len(json_bytes))
            full_packet = length_prefix + json_bytes
            
            # self.log(f"📤 SENDING: {len(json_bytes)} bytes of JSON data")
            # self.log(f"📤 JSON CONTENT: {json_data}")
            # self.log(f"📤 LENGTH PREFIX: {length_prefix}")
            # self.log(f"📤 FULL PACKET SIZE: {len(full_packet)} bytes")
            
            bytes_sent = self.socket.sendall(full_packet)
            # self.log(f"📤 SENDALL RESULT: {bytes_sent}")
            
            # Force flush the socket
            try:
                self.socket.setsockopt(socket.IPPROTO_TCP, socket.TCP_NODELAY, 1)
            except:
                pass
                
            return True
            
        except Exception as e:
            self.log(f"❌ Send error: {e}")
            self.disconnect()
            return False
    
    def start_game(self):
        """Send start game command to server"""
        if not self.connected:
            self.log("⚠️ Not connected to server")
            return
        
        self.log("🚀 Sending start_game command...")
        self.log(f"🔍 Socket state before sending: connected={self.connected}, socket={self.socket}")
        
        message = {
            "type": "game_command",
            "command": "start_game"
        }
        
        if self.send_message(message):
            self.log("✅ Start game command sent!")
            self.game_status = "Starting game..."
            self.game_started = True
            self.create_game_window()
        else:
            self.log("❌ Failed to send start game command")
    
    def send_key_event(self, key: str):
        """Send key event to server"""
        if not self.connected:
            return
        
        message = {
            "type": "key_event",
            "key": key
        }
        
        if self.send_message(message):
            self.log(f"⌨️  Key: {key}")
    
    def handle_events(self):
        """Handle pygame events"""
        for event in pygame.event.get():
            if event.type == pygame.QUIT:
                return False
            
            # Handle button events
            for button in self.buttons.values():
                button.handle_event(event)
            
            # Handle keyboard events
            if event.type == pygame.KEYDOWN:
                if event.key == pygame.K_a or event.key == pygame.K_LEFT:
                    self.send_key_event("a")
                elif event.key == pygame.K_d or event.key == pygame.K_RIGHT:
                    self.send_key_event("d")
                elif event.key == pygame.K_SPACE:
                    self.send_key_event("space")
                elif event.key == pygame.K_b:
                    self.send_key_event("b")
                elif event.key == pygame.K_v:
                    self.send_key_event("v")
                elif event.key == pygame.K_TAB:
                    # Toggle between controller and game window
                    if self.game_window:
                        self.close_game_window()
                    elif self.game_started:
                        self.create_game_window()
                elif event.key == pygame.K_RETURN:
                    if self.connected:
                        self.start_game()
                    else:
                        self.connect()
                elif event.key == pygame.K_ESCAPE:
                    return False
        
        return True
    
    def draw(self):
        """Draw the appropriate window"""
        if self.game_window and self.game_screen:
            # Draw game window
            self.draw_game_window()
        else:
            # Draw controller window
            self.draw_controller_window()
    def draw_controller_window(self):
        """Draw the controller UI"""
        # Draw visually distinct vertical gradient background
        top_color = (44, 62, 80)        # Deep blue
        bottom_color = (200, 220, 255)  # Light silver-blue
        for y in range(WINDOW_HEIGHT):
            ratio = y / WINDOW_HEIGHT
            r = int(top_color[0] * (1 - ratio) + bottom_color[0] * ratio)
            g = int(top_color[1] * (1 - ratio) + bottom_color[1] * ratio)
            b = int(top_color[2] * (1 - ratio) + bottom_color[2] * ratio)
            pygame.draw.line(self.screen, (r, g, b), (0, y), (WINDOW_WIDTH, y))

        # Enhanced Title: larger, bold, centered, with shadow
        title_text = " Arkanoid "
        big_title_font = pygame.font.Font(None, 64)
        title_y = 120  # Lowered from 70 to 120
        # Shadow
        shadow_surface = big_title_font.render(title_text, True, (0, 0, 0))
        shadow_rect = shadow_surface.get_rect(center=(WINDOW_WIDTH // 2, title_y))
        self.screen.blit(shadow_surface, (shadow_rect.x + 3, shadow_rect.y + 3))
        # Main title
        title_surface = big_title_font.render(title_text, True, COLORS['TEXT_WHITE'])
        title_rect = title_surface.get_rect(center=(WINDOW_WIDTH // 2, title_y))
        self.screen.blit(title_surface, title_rect)

        # Add extra spacing below the title
        menu_y_offset = 140

        # Add extra spacing before buttons
        button_y_offset = menu_y_offset

        # Draw 'Connect' and 'START GAME' buttons (vertically centered)
        button_keys = ['connect', 'start_game']
        for key in button_keys:
            if key in self.buttons:
                button = self.buttons[key]
                button.draw(self.screen, self.font)

        # (Game status and instructions removed for a cleaner main menu)
        pygame.display.flip()
    
    def draw_game_window(self):
        """Draw the game window with game objects"""
        # Guard: ensure the game surface exists
        if not getattr(self, 'game_screen', None):
            return

        # Clear game screen (keep overall dark)
        self.game_screen.fill((30, 35, 45))

        # Game title
        title_text = "Arkanoid"
        title_font = pygame.font.Font(None, 48)
        title_surface = title_font.render(title_text, True, DARK_WHITE)
        title_rect = title_surface.get_rect(center=(400, 30))
        self.game_screen.blit(title_surface, title_rect)

        # Show only the level centered under the title (remove previous score/lives status)
        try:
            level_text = f"Level {self.level}"
            level_surf = self.font.render(level_text, True, DARK_WHITE)
            level_rect = level_surf.get_rect(center=(400, 70))
            self.game_screen.blit(level_surf, level_rect)
        except Exception:
            pass

        # Draw hearts (lives) top-left
        try:
            self._draw_hearts(self.game_screen, 20, 20, size=22)
        except Exception:
            pass

        # Draw inline 'SCORE:' and numeric score on the top-right
        try:
            pad = 20
            label_font = self.title_font
            value_font = self.title_font

            label = "SCORE:"
            value = str(self.score)

            label_surf = label_font.render(label, True, COLORS['TEXT_WHITE'])
            value_surf = value_font.render(value, True, (255, 240, 120))

            # Place both on the same top-right baseline
            right_x = self.game_screen.get_width() - pad
            # value anchored to right_x, label to the left of it with small gap
            value_rect = value_surf.get_rect(topright=(right_x, 12))
            label_rect = label_surf.get_rect(topright=(value_rect.left - 8, 12))

            self.game_screen.blit(label_surf, label_rect)
            self.game_screen.blit(value_surf, value_rect)
        except Exception:
            pass

        # Draw bright game area rectangle
        game_area = pygame.Rect(0, 100, 800, 500)
        pygame.draw.rect(self.game_screen, (240, 240, 255), game_area)  # Bright game area
        pygame.draw.rect(self.game_screen, (100, 100, 120), game_area, 2)

        # Draw game objects inside the game area
        self.draw_game_objects_on_game_screen(game_area)

        # Ensure display is updated
        # If game over, draw a centered large caption above everything
        try:
            if getattr(self, 'game_over', False):
                go_font = pygame.font.Font(None, 96)
                go_surf = go_font.render("GAME OVER", True, (230, 40, 40))
                # shadow
                shadow = go_font.render("GAME OVER", True, (0, 0, 0))
                cx = self.game_screen.get_width() // 2
                cy = self.game_screen.get_height() // 2
                shadow_rect = shadow.get_rect(center=(cx + 4, cy + 4))
                go_rect = go_surf.get_rect(center=(cx, cy))
                self.game_screen.blit(shadow, shadow_rect)
                self.game_screen.blit(go_surf, go_rect)
        except Exception:
            pass

        pygame.display.flip()

    def _draw_hearts(self, surface, x, y, size=20):
        """Draw hearts representing lives starting at (x,y)."""
        # simple heart made from two circles and a triangle
        spacing = size + 6
        for i in range(max(0, int(self.lives))):
            hx = x + i * spacing
            hy = y
            # draw two circles
            r = size // 3
            cx1 = hx + r
            cx2 = hx + r * 3
            cy = hy + r
            pygame.draw.circle(surface, COLORS['TEXT_RED'], (cx1, cy), r)
            pygame.draw.circle(surface, COLORS['TEXT_RED'], (cx2, cy), r)
            # triangle
            points = [(hx, cy), (hx + r*4, cy), (hx + r*2, cy + r*2)]
            pygame.draw.polygon(surface, COLORS['TEXT_RED'], points)

    def _make_metallic_paddle_surface(self, width, height, base_color=(100,150,255)):
        """Create and cache a metallic-looking rounded paddle surface."""
        key = (width, height, base_color)
        if key in self._paddle_surface_cache:
            return self._paddle_surface_cache[key]

        surf = pygame.Surface((width, height), pygame.SRCALPHA)

        # Background rounded rect (darker base)
        darker = tuple(max(0, c - 30) for c in base_color)
        pygame.draw.rect(surf, darker, pygame.Rect(0, height//6, width, height*4//6), border_radius=height//3)

        # Main glossy body (slightly lighter gradient)
        for i in range(height//2):
            t = i / max(1, (height//2 - 1))
            r = int(darker[0] * (1 - t) + base_color[0] * t)
            g = int(darker[1] * (1 - t) + base_color[1] * t)
            b = int(darker[2] * (1 - t) + base_color[2] * t)
            line_y = height//6 + i
            pygame.draw.line(surf, (r, g, b, 255), (4, line_y), (width-4, line_y))

        # Specular highlight: a thin semi-transparent white streak
        hl = pygame.Surface((width, height), pygame.SRCALPHA)
        grad_w = max(6, width // 6)
        for i in range(grad_w):
            alpha = int(180 * (1 - (i / grad_w))**1.6)
            pygame.draw.line(hl, (255, 255, 255, alpha), (i + 6, height//4), (i + 6, height//2))
        surf.blit(hl, (0, 0), special_flags=pygame.BLEND_PREMULTIPLIED)

        # Thin border
        pygame.draw.rect(surf, (220, 220, 220, 120), surf.get_rect(), width=2, border_radius=height//3)

        self._paddle_surface_cache[key] = surf
        return surf

    def _draw_paddle(self, surface, paddle_obj, game_area, dt=1/60):
        """Draw a metallic paddle with soft bevel and hit/velocity effects.

        paddle_obj is expected to have 'pos' and optional 'w' (width) and 'h' (height).
        dt is delta time in seconds used to decay flash/animation.
        """
        # Compute paddle properties
        pos = paddle_obj.get('pos', {}) if isinstance(paddle_obj, dict) else {}
        if pos and isinstance(pos, dict):
            x = pos.get('x', 0) or pos.get(1, 0)
            y = pos.get('y', 0) or pos.get(2, 0)
        elif isinstance(pos, (list, tuple)) and len(pos) >= 2:
            x, y = pos[0], pos[1]
        else:
            x, y = 400, 550

        paddle_w = int(paddle_obj.get('w', paddle_obj.get('width', 100)))
        paddle_h = int(paddle_obj.get('h', paddle_obj.get('height', 20)))

        # Map to screen coords
        screen_x = int(game_area.x + x * (game_area.width / 800))
        screen_y = int(game_area.y + y * (game_area.height / 600))

        # track velocity for subtle motion effect
        if self.paddle_last_x is None:
            self.paddle_last_x = screen_x
        vx = (screen_x - self.paddle_last_x) / max(1e-6, dt)
        self.paddle_velocity = 0.85 * self.paddle_velocity + 0.15 * vx
        self.paddle_last_x = screen_x

        # Hit flash decay
        if self.paddle_hit_flash > 0:
            self.paddle_hit_flash = max(0.0, self.paddle_hit_flash - dt * 8.0)

        # Create base paddle surface
        surf = self._make_metallic_paddle_surface(paddle_w, paddle_h)

        # Create a working copy so we can tint/flash
        draw_surf = surf.copy()

        # Velocity-based tint removed to avoid whitening during movement.
        # Movement will still affect paddle_velocity internally but won't change appearance.

        # Hit flash overlay (brighten briefly)
        if self.paddle_hit_flash > 0.01:
            alpha = int(160 * self.paddle_hit_flash)
            flash = pygame.Surface(draw_surf.get_size(), pygame.SRCALPHA)
            # Slightly darker off-white (soft grayish-white) instead of pure white
            flash.fill((235, 235, 235, alpha))
            draw_surf.blit(flash, (0, 0), special_flags=pygame.BLEND_RGBA_ADD)

        # Draw shadow and blit to main surface
        dest_rect = draw_surf.get_rect(center=(screen_x, screen_y))
        shadow = pygame.Surface((dest_rect.width + 8, dest_rect.height + 8), pygame.SRCALPHA)
        pygame.draw.ellipse(shadow, (0, 0, 0, 80), shadow.get_rect())
        sh_rect = shadow.get_rect(center=(screen_x + 4, screen_y + 6))
        surface.blit(shadow, sh_rect)

        surface.blit(draw_surf, dest_rect)
    
    def draw_game_objects_on_game_screen(self, game_area):
        """Draw game objects (ball, paddle, bricks) on the game screen"""
        # Scale factor to fit game coordinates to screen
        scale_x = game_area.width / 800  # Assuming game world is 800 pixels wide
        scale_y = game_area.height / 600  # Assuming game world is 600 pixels tall
        
        # Draw balls (multiple balls support)
        # Energy Core ball visuals (glowing core + bloom + trail)
        # Prepare per-ball trail buffers if missing
        if not hasattr(self, '_ball_trails'):
            self._ball_trails = {}
        if not hasattr(self, '_ball_pulse'):
            self._ball_pulse = {}

        def _make_glow_surfaces(radius, color=(255, 200, 60)):
            # cache by (radius, color)
            key = ('glow', radius, color)
            if not hasattr(self, '_surface_cache'):
                self._surface_cache = {}
            if key in self._surface_cache:
                return self._surface_cache[key]

            # Core with sharper bright center and subtle rim
            core = pygame.Surface((radius*2, radius*2), pygame.SRCALPHA)
            for r in range(radius, 0, -1):
                t = (radius - r) / max(1, radius - 1)
                rr = int(color[0] * (1 - 0.25 * t))
                gg = int(color[1] * (1 - 0.25 * t))
                bb = int(color[2] * (1 - 0.25 * t))
                alpha = int(255 * (1 - t**1.5))
                pygame.draw.circle(core, (rr, gg, bb, alpha), (radius, radius), r)

            # Thin colored rim (no white halo) — draws ring only
            rim_r = max(1, int(radius * 1.2))
            rim = pygame.Surface((rim_r*2, rim_r*2), pygame.SRCALPHA)
            rim_thickness = max(1, radius // 3)
            rim_color = (max(0, int(color[0]*0.9)), max(0, int(color[1]*0.9)), max(0, int(color[2]*0.9)), 140)
            pygame.draw.circle(rim, rim_color, (rim_r, rim_r), rim_r - rim_thickness//2, rim_thickness)

            self._surface_cache[key] = (core, rim)
            return core, rim

        for i, ball in enumerate(self.game_objects['balls']):
            ball_id = ball.get('id', f'ball_{i+1}')
            pos = ball.get('pos', {})
            if pos and isinstance(pos, dict):
                x = pos.get('x', 0) or pos.get(1, 0)
                y = pos.get('y', 0) or pos.get(2, 0)
            elif isinstance(pos, (list, tuple)) and len(pos) >= 2:
                x, y = pos[0], pos[1]
            else:
                x, y = 400, 300

            screen_x = int(game_area.x + x * scale_x)
            screen_y = int(game_area.y + y * scale_y)

            # radius may be communicated from server; default to 10
            radius = int(ball.get('r', ball.get('radius', 10)))

            # choose glow color; default warm yellow, can vary by ball index
            glow_color = ball.get('color', (255, 200, 60)) if isinstance(ball.get('color', None), tuple) else (255, 200, 60)

            # Use configured glow color (no temporary overrides)
            glow_color = ball.get('color', (255, 200, 60)) if isinstance(ball.get('color', None), tuple) else (255, 200, 60)

            core_surf, rim_surf = _make_glow_surfaces(radius, glow_color)

            # Draw trail (store last positions) — subtle, darker-than-white tinted dots
            trail = self._ball_trails.setdefault(ball_id, [])
            trail.insert(0, (screen_x, screen_y))
            # keep trail very short for subtlety
            if len(trail) > 3:
                trail.pop()

            # Render trail as small dark translucent dots (not white), subtle and short
            for t_idx, (tx, ty) in enumerate(trail[1:3]):
                frac = 1 - (t_idx / max(1, 2))
                a = int(42 * frac)  # slightly stronger alpha but dark tint
                size_mult = 0.28 * (0.9 + 0.1 * frac)
                tw = max(3, int(core_surf.get_width() * size_mult))
                th = max(3, int(core_surf.get_height() * size_mult))
                ts = pygame.transform.smoothscale(core_surf, (tw, th))
                # Create a dark tint (use ~20% of glow color but biased toward darker grayscale)
                avg = int((glow_color[0] + glow_color[1] + glow_color[2]) / 3)
                dark_shade = int(avg * 0.25)
                tint_color = (dark_shade, dark_shade, dark_shade, a)
                tint = pygame.Surface(ts.get_size(), pygame.SRCALPHA)
                tint.fill(tint_color)
                ts.blit(tint, (0, 0), special_flags=pygame.BLEND_RGBA_MULT)
                tr = ts.get_rect(center=(tx, ty))
                # Use normal blit (not additive) so trail appears as a dark smear behind the ball
                self.game_screen.blit(ts, tr)

            # Pulse on recent hit
            pulse = self._ball_pulse.get(ball_id, 0.0)
            if pulse > 0:
                self._ball_pulse[ball_id] = max(0.0, pulse - 0.08)
                scale = 1.0 + 0.35 * pulse
            else:
                scale = 1.0

            # Blit bloom (additive)
            bw = int(rim_surf.get_width() * scale)
            bh = int(rim_surf.get_height() * scale)
            bs = pygame.transform.smoothscale(rim_surf, (bw, bh))
            brect = bs.get_rect(center=(screen_x, screen_y))
            self.game_screen.blit(bs, brect, special_flags=pygame.BLEND_ADD)

            # Blit core
            cw = int(core_surf.get_width() * scale)
            ch = int(core_surf.get_height() * scale)
            cs = pygame.transform.smoothscale(core_surf, (cw, ch))
            crect = cs.get_rect(center=(screen_x, screen_y))
            self.game_screen.blit(cs, crect)
        
        # Prepare paddle rect for collision checks (if paddle exists)
        paddle_rect_for_collision = None
        if self.game_objects.get('paddle'):
            p = self.game_objects['paddle']
            ppos = p.get('pos', {}) if isinstance(p, dict) else {}
            if ppos and isinstance(ppos, dict):
                px = ppos.get('x', 400) or ppos.get(1, 400)
                py = ppos.get('y', 550) or ppos.get(2, 550)
            elif isinstance(ppos, (list, tuple)) and len(ppos) >= 2:
                px, py = ppos[0], ppos[1]
            else:
                px, py = 400, 550
            pw = int(self.game_objects['paddle'].get('w', self.game_objects['paddle'].get('width', 100)) * (game_area.width / 800))
            ph = int(self.game_objects['paddle'].get('h', self.game_objects['paddle'].get('height', 20)) * (game_area.height / 600))
            p_screen_x = int(game_area.x + px * (game_area.width / 800))
            p_screen_y = int(game_area.y + py * (game_area.height / 600))
            paddle_rect_for_collision = pygame.Rect(p_screen_x - pw//2, p_screen_y - ph//2, pw, ph)

        # Draw bombs (falling red circles with explosion symbol)
        bombs = self.game_objects.get('bombs', [])
        for i, bomb in enumerate(bombs):
            pos = bomb.get('pos', {})
            if pos and isinstance(pos, dict):
                x = pos.get('x', 0) or pos.get(1, 0)  # Handle tuple format
                y = pos.get('y', 0) or pos.get(2, 0)
            elif isinstance(pos, (list, tuple)) and len(pos) >= 2:
                x, y = pos[0], pos[1]
            else:
                x, y = 400, 100  # Default top center
                
            screen_x = int(game_area.x + x * scale_x)
            screen_y = int(game_area.y + y * scale_y)
            
            # Draw bomb as red circle with black border
            bomb_color = (220, 20, 20)  # Dark red
            border_color = (0, 0, 0)    # Black border

            pygame.draw.circle(self.game_screen, bomb_color, (screen_x, screen_y), 10)
            pygame.draw.circle(self.game_screen, border_color, (screen_x, screen_y), 10, 2)
            
            # Draw explosion symbol (💣) or "B" for bomb
            font = pygame.font.Font(None, 20)
            bomb_symbol = font.render("💣", True, DARK_WHITE)
            if bomb_symbol.get_width() == 0:  # Fallback if emoji not supported
                bomb_symbol = font.render("B", True, DARK_WHITE)
            symbol_rect = bomb_symbol.get_rect(center=(screen_x, screen_y))
            self.game_screen.blit(bomb_symbol, symbol_rect)
            
            # Draw bomb ID for debugging
            bomb_id = bomb.get('id', f'bomb_{i+1}')
            font_small = pygame.font.Font(None, 16)
            id_text = font_small.render(str(bomb_id), True, DARK_WHITE)
            id_rect = id_text.get_rect(center=(screen_x, screen_y + 15))
            self.game_screen.blit(id_text, id_rect)
        
        # Draw viruses (bouncing green circles with virus symbol)
        viruses = self.game_objects.get('viruses', [])
        for i, virus in enumerate(viruses):
            pos = virus.get('pos', {})
            if pos and isinstance(pos, dict):
                x = pos.get('x', 0) or pos.get(1, 0)  # Handle tuple format
                y = pos.get('y', 0) or pos.get(2, 0)
            elif isinstance(pos, (list, tuple)) and len(pos) >= 2:
                x, y = pos[0], pos[1]
            else:
                x, y = 400, 300  # Default center
            
            # Convert to screen coordinates
            screen_x = int(x)
            screen_y = int(y)
            
            # Draw virus as green circle with dark green border
            virus_color = (50, 200, 50)    # Bright green
            border_color = (20, 100, 20)   # Dark green border

            pygame.draw.circle(self.game_screen, virus_color, (screen_x, screen_y), 8)
            pygame.draw.circle(self.game_screen, border_color, (screen_x, screen_y), 8, 2)
            
            # Draw virus symbol (🦠) or "V" for virus
            font = pygame.font.Font(None, 18)
            virus_symbol = font.render("🦠", True, DARK_WHITE)
            if virus_symbol.get_width() == 0:  # Fallback if emoji not supported
                virus_symbol = font.render("V", True, DARK_WHITE)
            symbol_rect = virus_symbol.get_rect(center=(screen_x, screen_y))
            self.game_screen.blit(virus_symbol, symbol_rect)
            
            # Draw virus ID for debugging
            virus_id = virus.get('id', f'virus_{i+1}')
            font_small = pygame.font.Font(None, 16)
            id_text = font_small.render(str(virus_id), True, DARK_WHITE)
            id_rect = id_text.get_rect(center=(screen_x, screen_y + 12))
            self.game_screen.blit(id_text, id_rect)
        
        # If there is paddle, detect collisions with balls to trigger hit flash
        if paddle_rect_for_collision is not None:
            current_collisions = set()
            for i, ball in enumerate(self.game_objects['balls']):
                pos = ball.get('pos', {})
                if pos and isinstance(pos, dict):
                    bx = pos.get('x', 0) or pos.get(1, 0)
                    by = pos.get('y', 0) or pos.get(2, 0)
                elif isinstance(pos, (list, tuple)) and len(pos) >= 2:
                    bx, by = pos[0], pos[1]
                else:
                    bx, by = 0, 0

                b_screen_x = int(game_area.x + bx * scale_x)
                b_screen_y = int(game_area.y + by * scale_y)
                if paddle_rect_for_collision.collidepoint(b_screen_x, b_screen_y):
                    current_collisions.add(i)

            # New collisions = current - previous
            new_hits = current_collisions - self._ball_prev_collisions
            if new_hits:
                self.paddle_hit_flash = 1.0
                # set pulse for each newly-hit ball
                for idx in new_hits:
                    if idx < len(self.game_objects['balls']):
                        b = self.game_objects['balls'][idx]
                        bid = b.get('id', f'ball_{idx+1}')
                        self._ball_pulse[bid] = 1.0
                        # (no color override on hit; keep ball color unchanged)
            self._ball_prev_collisions = current_collisions

        # Draw paddle — use new metallic paddle renderer (Option A)
        if self.game_objects['paddle']:
            # pass dt estimated from FPS
            dt = 1.0 / max(1, FPS)
            self._draw_paddle(self.game_screen, self.game_objects['paddle'], game_area, dt=dt)
        
        # Glass brick renderer (modern glass blocks)
        def _make_glass_brick_surface(w, h, color, hp):
            # cache keyed by (w,h,color,hp_state)
            key = ('glass_brick', w, h, color, max(0, int(hp)))
            if not hasattr(self, '_brick_surface_cache'):
                self._brick_surface_cache = {}
            if key in self._brick_surface_cache:
                return self._brick_surface_cache[key]

            surf = pygame.Surface((w, h), pygame.SRCALPHA)

            # Base: slightly translucent fill
            base = (*color[:3], 200)
            pygame.draw.rect(surf, base, pygame.Rect(0, 0, w, h), border_radius=6)

            # Inner lighter panel to simulate thickness (muted, darker highlight)
            inner = tuple(min(255, int(c * 1.06)) for c in color[:3]) + (64,)
            pad = max(4, int(min(w, h) * 0.08))
            pygame.draw.rect(surf, inner, pygame.Rect(pad, pad, w - pad*2, h - pad*2), border_radius=4)

            # Soft inner glow (low alpha radial by blitting scaled circles)
            glow = pygame.Surface((w, h), pygame.SRCALPHA)
            gx, gy = w//2, h//2
            max_r = max(w, h)
            for r in range(max_r, 0, -6):
                a = int(18 * (1 - (r / max_r)))
                col = (color[0], color[1], color[2], a)
                pygame.draw.circle(glow, col, (gx, gy), r)
            surf.blit(glow, (0,0), special_flags=pygame.BLEND_PREMULTIPLIED)

            # Rim highlight: muted off-white outline and darker feather
            rim = pygame.Surface((w, h), pygame.SRCALPHA)
            pygame.draw.rect(rim, (220,220,220,64), pygame.Rect(1,1,w-2,h-2), width=2, border_radius=6)
            # feather the rim by blitting slightly offset darker border for depth
            pygame.draw.rect(rim, (10,10,10,80), pygame.Rect(0,0,w,h), width=1, border_radius=6)
            surf.blit(rim, (0,0), special_flags=pygame.BLEND_PREMULTIPLIED)

            # (crack overlay removed — bricks remain uncracked at all HP levels)

            # Final subtle border to separate from background (muted)
            pygame.draw.rect(surf, (110,110,120,140), surf.get_rect(), width=1, border_radius=6)

            self._brick_surface_cache[key] = surf
            return surf

        # Draw bricks using glass style
        for brick in self.game_objects['bricks']:
            pos = brick.get('pos', {})
            if pos and isinstance(pos, dict):
                x = pos.get('x', 0) or pos.get(1, 0)
                y = pos.get('y', 0) or pos.get(2, 0)
            elif isinstance(pos, (list, tuple)) and len(pos) >= 2:
                x, y = pos[0], pos[1]
            else:
                continue

            screen_x = int(game_area.x + x * scale_x)
            screen_y = int(game_area.y + y * scale_y)
            brick_width = int(75 * scale_x)  # Scaled brick width
            brick_height = int(25 * scale_y)  # Scaled brick height

            # Color based on HP level
            brick_color = get_brick_color_by_hp(brick)
            hp = brick.get('hp', brick.get('health', 3))

            bsurf = _make_glass_brick_surface(brick_width, brick_height, brick_color, hp)
            brect = bsurf.get_rect(center=(screen_x, screen_y))
            self.game_screen.blit(bsurf, brect)

            # HP text removed: bricks render without numeric labels per user request
    
    def draw_game_objects(self):
        """Legacy method - kept for compatibility"""
        # Define game area (center of screen)
        game_area = pygame.Rect(50, 50, WINDOW_WIDTH - 100, WINDOW_HEIGHT - 250)
        pygame.draw.rect(self.screen, (40, 40, 60), game_area)
        pygame.draw.rect(self.screen, COLORS['BORDER'], game_area, 2)
        
        # Scale factor to fit game coordinates to screen
        scale_x = game_area.width / 800  # Assuming game world is 800 pixels wide
        scale_y = game_area.height / 600  # Assuming game world is 600 pixels tall
        
        # Draw balls (multiple balls support)
        ball_colors = [(0, 0, 0), (255, 0, 0), (0, 255, 0), (0, 0, 255), (255, 255, 0), (255, 0, 255), (0, 255, 255), (255, 128, 0)]
        
        for i, ball in enumerate(self.game_objects['balls']):
            pos = ball.get('pos', {})
            if pos and isinstance(pos, dict):
                x = pos.get('x', 0) or pos.get(1, 0)  # Handle tuple format
                y = pos.get('y', 0) or pos.get(2, 0)
            elif isinstance(pos, (list, tuple)) and len(pos) >= 2:
                x, y = pos[0], pos[1]
            else:
                x, y = 400, 300  # Default center
                
            screen_x = int(game_area.x + x * scale_x)
            screen_y = int(game_area.y + y * scale_y)
            
            # Use different colors for different balls
            color_index = i % len(ball_colors)
            ball_color = ball_colors[color_index]
            pygame.draw.circle(self.screen, ball_color, (screen_x, screen_y), 8)
        
        # Draw paddle
        if self.game_objects['paddle']:
            paddle = self.game_objects['paddle']
            pos = paddle.get('pos', {})
            if pos and isinstance(pos, dict):
                x = pos.get('x', 0) or pos.get(1, 0)
                y = pos.get('y', 0) or pos.get(2, 0)
            elif isinstance(pos, (list, tuple)) and len(pos) >= 2:
                x, y = pos[0], pos[1]
            else:
                x, y = 400, 550  # Default bottom center
                
            screen_x = int(game_area.x + x * scale_x)
            screen_y = int(game_area.y + y * scale_y)
            paddle_width = int(80 * scale_x)  # Scaled paddle width
            paddle_height = int(15 * scale_y)  # Scaled paddle height
            paddle_rect = pygame.Rect(screen_x - paddle_width//2, screen_y - paddle_height//2, paddle_width, paddle_height)
            pygame.draw.rect(self.screen, (100, 150, 255), paddle_rect)
        
        # Draw bricks
        for brick in self.game_objects['bricks']:
            pos = brick.get('pos', {})
            if pos and isinstance(pos, dict):
                x = pos.get('x', 0) or pos.get(1, 0)
                y = pos.get('y', 0) or pos.get(2, 0)
            elif isinstance(pos, (list, tuple)) and len(pos) >= 2:
                x, y = pos[0], pos[1]
            else:
                continue
                
            screen_x = int(game_area.x + x * scale_x)
            screen_y = int(game_area.y + y * scale_y)
            brick_width = int(60 * scale_x)  # Scaled brick width
            brick_height = int(20 * scale_y)  # Scaled brick height
            brick_rect = pygame.Rect(screen_x - brick_width//2, screen_y - brick_height//2, brick_width, brick_height)
            
            # Color based on HP level
            brick_color = get_brick_color_by_hp(brick)
            
            pygame.draw.rect(self.screen, brick_color, brick_rect)
            pygame.draw.rect(self.screen, (200, 200, 200), brick_rect, 1)
            
            # Numeric HP removed from legacy brick drawing per user request
    
    def run(self):
        """Main game loop"""
        self.log("🎮 Arkanoid GUI started")
        self.log(f"📡 Ready to connect to {SERVER_HOST}:{SERVER_PORT}")
        self.log("💡 Click Connect, then START GAME to begin!")
        
        while self.running:
            current_time = time.time()
            
            # Auto-connect attempt
            if not self.connected and current_time > self.auto_connect_time:
                self.auto_connect_time = float('inf')  # Only try once
                self.connect()
            
            # Handle events
            if not self.handle_events():
                self.running = False
                break
            
            # Draw
            self.draw()
            
            # Control framerate
            self.clock.tick(FPS)
        
        # Cleanup
        self.disconnect()
        pygame.quit()
        sys.exit()

def main():
    """Main entry point"""
    try:
        print("🎮 Starting Arkanoid GUI with Pygame...")
        app = ArkanoidGUI()
        app.run()
    except KeyboardInterrupt:
        print("\n👋 Goodbye!")
    except Exception as e:
        print(f"❌ Error: {e}")
        import traceback
        traceback.print_exc()

if __name__ == "__main__":
    main()