{
  lib,
  config,
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
      NIXOS_OZONE_WL = "1";
      MOZ_ENABLE_WAYLAND = "1";
      SDL_VIDEODRIVER = "wayland";
      GDK_BACKEND = "wayland,x11,*";
      CLUTTER_BACKEND = "wayland";
      QT_QPA_PLATFORM = "wayland;xcb";
      QT_WAYLAND_DISABLE_WINDOWDECORATION = "1";
      QT_AUTO_SCREEN_SCALE_FACTOR = "1";
      # desktop type
      XDG_CURRENT_DESKTOP = "niri";
      XDG_SESSION_DESKTOP = "niri";
    };

    # startup
    spawn-at-startup = [
      {command = ["${lib.getExe pkgs.networkmanagerapplet}"];}
      {command = ["${lib.getExe pkgs.wlsunset}" "-T" "5200"];}
      {command = ["${lib.getExe pkgs.swaybg}" "-i" "${def.wallpaper}" "-m" "fill"];}
    ];

    switch-events = with config.lib.niri.actions; let
      sh = spawn "sh" "-c";
    in {
      tablet-mode-on.action = sh "notify-send tablet-mode-on";
      tablet-mode-off.action = sh "notify-send tablet-mode-off";
      lid-open.action = sh "notify-send lid-open";
      lid-close.action = sh "notify-send lid-close";
    };

    # keybinds
    binds = with config.lib.niri.actions; let
      sh = spawn "sh" "-c";
      msg = command: sh "niri msg action ${command}";
    in {
      # programs
      "Mod+Return".action = spawn "kitty";
      "Mod+E".action = spawn "thunar";
      "Mod+Space".action = spawn "fuzzel";
      "Super+Shift+L".action = spawn "swaylock";
      "Super+Shift+P".action = spawn "wlogout";
      "Mod+W".action = spawn "systemctl --user restart waybar.service";
      # media keys
      "XF86AudioRaiseVolume".action = spawn "wpctl set-volume @DEFAULT_AUDIO_SINK@ 0.1+";
      "XF86AudioLowerVolume".action = spawn "wpctl set-volume @DEFAULT_AUDIO_SINK@ 0.1-";
      "XF86AudioMute".action = spawn "wpctl set-mute @DEFAULT_AUDIO_SINK@ toggle";
      "XF86AudioMicMute".action = spawn "wpctl set-mute @DEFAULT_AUDIO_SOURCE@ toggle";
      "XF86MonBrightnessUp".action = spawn "brightnessctl --device=amdgpu_bl1 s 5%+";
      "XF86MonBrightnessDown".action = spawn "brightnessctl --device=amdgpu_bl1 s 5%-";
      # screenshot
      "Print".action = screenshot;
      "Ctrl+Print".action = screenshot-screen;
      "Alt+Print".action = screenshot-window;
      # window actions
      "Mod+Q".action = close-window;
      "Ctrl+Alt+Delete".action = quit;
      "Mod+Left".action = focus-column-left;
      "Mod+Right".action = focus-column-right;
      "Mod+Up".action = focus-workspace-up;
      "Mod+Down".action = focus-workspace-down;
      "Mod+Shift+Left".action = move-column-left;
      "Mod+Shift+Right".action = move-column-right;
      "Mod+Shift+Up".action = move-window-to-workspace-up;
      "Mod+Shift+Down".action = move-window-to-workspace-down;
      # window presets && row/col manipulation
      "Mod+R".action = switch-preset-column-width;
      "Mod+M".action = maximize-column;
      "Mod+Shift+M".action = fullscreen-window;
      "Mod+Comma".action = consume-window-into-column;
      "Mod+Period".action = expel-window-from-column;
      "Mod+Minus".action = set-column-width "-10%";
      "Mod+Equal".action = set-column-width "+10%";
      "Mod+Shift+Minus".action = set-window-height "-10%";
      "Mod+Shift+Equal".action = set-window-height "+10%";
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
      gaps = 5;
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
