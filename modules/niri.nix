{
  lib,
  pkgs,
  inputs,
  def,
  ...
}: {
  programs.niri.settings = {
    # general
    prefer-no-csd = true;
    hotkey-overlay.skip-at-startup = true;
    screenshot-path = "~/Pictures/Screenshots/Screenshot from %Y-%m-%d %H-%M-%S.png";
    # env vars
    environment = {
      NIXOS_OZONE_WL = 1;
      MOZ_ENABLE_WAYLAND = 1;

      XDG_CURRENT_DESKTOP = "Hyprland";
      XDG_SESSION_TYPE = "wayland";
      XDG_SESSION_DESKTOP = "Hyprland";

      GDK_BACKEND = "wayland,x11,*";
      CLUTTER_BACKEND = "wayland";
      QT_QPA_PLATFORM = "wayland;xcb";
      QT_WAYLAND_DISABLE_WINDOWDECORATION = 1;
      QT_AUTO_SCREEN_SCALE_FACTOR = 1;

      SDL_VIDEODRIVER = "x11";
      DISPLAY = ":0";
    };

    # startup
    spawn-at-startup = [
      {command = ["${lib.getExe pkgs.networkmanagerapplet}"];}
      {command = ["${lib.getExe pkgs.wlsunset}" "-T" "5200"];}
      {command = ["${lib.getExe pkgs.swaybg}" "-i" "${def.wallpaper}" "-m" "fill"];}
    ];
    # keybinds
    binds = let
      spawn = command: {action.spawn = ["sh" "-c" ''${command}''];};
      action = command: {action.spawn = ["sh" "-c" ''niri msg action ${command}''];};
    in {
      # programs
      "Mod+Return" = spawn "kitty";
      "Mod+E" = spawn "thunar";
      "Mod+Space" = spawn "fuzzel";
      "Super+Shift+L" = spawn "swaylock";
      "Super+Shift+P" = spawn "wlogout";
      "Mod+W" = spawn "systemctl --user restart waybar.service";
      # media keys
      "XF86AudioRaiseVolume" = spawn "wpctl set-volume @DEFAULT_AUDIO_SINK@ 0.1+";
      "XF86AudioLowerVolume" = spawn "wpctl set-volume @DEFAULT_AUDIO_SINK@ 0.1-";
      "XF86AudioMute" = spawn "wpctl set-mute @DEFAULT_AUDIO_SINK@ toggle";
      "XF86AudioMicMute" = spawn "wpctl set-mute @DEFAULT_AUDIO_SOURCE@ toggle";
      "XF86MonBrightnessUp" = spawn "brightnessctl --device=amdgpu_bl1 s 5%+";
      "XF86MonBrightnessDown" = spawn "brightnessctl --device=amdgpu_bl1 s 5%-";
      # screenshot
      "Print" = spawn "screenshot";
      "Ctrl+Print" = action "screenshot-screen";
      "Alt+Print" = action "screenshot-window";
      # window actions
      "Mod+Q" = action "close-window";
      "Ctrl+Alt+Delete" = action "quit";
      "Mod+Left" = action "focus-column-left";
      "Mod+Right" = action "focus-column-right";
      "Mod+Up" = action "focus-workspace-up";
      "Mod+Down" = action "focus-workspace-down";
      "Mod+Shift+Left" = action "move-column-left";
      "Mod+Shift+Right" = action "move-column-right";
      "Mod+Shift+Up" = action "move-window-to-workspace-up";
      "Mod+Shift+Down" = action "move-window-to-workspace-down";
      # window presets && row/col manipulation
      "Mod+R" = action "switch-preset-column-width";
      "Mod+M" = action "maximize-column";
      "Mod+Shift+M" = action "fullscreen-window";
      "Mod+Comma" = action "consume-window-into-column";
      "Mod+Period" = action "expel-window-from-column";
      "Mod+Minus" = action "set-column-width -10%";
      "Mod+Plus" = action "set-column-width +10%";
      "Mod+Shift+Minus" = action "set-window-height -10%";
      "Mod+Shift+Plus" = action "set-window-height +10%";
    };
    # input
    input = {
      keyboard.xkb.layout = "${def.layout}";
      mouse.accel-speed = 1.0;
      touchpad = {
        tap = true;
        dwt = true;
        natural-scroll = true;
        click-method = "clickfinger";
      };
      tablet.map-to-output = "eDP-1";
      touch.map-to-output = "eDP-1";
    };
    # monitors
    outputs."DP-2" = {
      enable = true;
      mode = {
        width = 1920;
        height = 1080;
        refresh = 165.001;
      };
      position.x = 0;
      position.y = 0;
    };
    # layout n theming
    layout = {
      gaps = 8;
      border.width = 2;
      always-center-single-column = false;
    };
    window-rules = [
      {
        geometry-corner-radius = let
          r = 3.0;
        in {
          top-left = r;
          top-right = r;
          bottom-left = r;
          bottom-right = r;
        };
        clip-to-geometry = true;
      }
    ];
  };
}
