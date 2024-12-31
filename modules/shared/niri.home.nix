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
    in {
      # programs
      "Mod+Return".action = spawn "kitty";
      "Mod+E".action = spawn "thunar";
      "Mod+Space".action = spawn "fuzzel";
      "Super+Shift+L".action = spawn "swaylock";
      "Super+Shift+P".action = spawn "wlogout";
      "Mod+W".action = sh ''systemctl --user restart waybar.service'';
      # clipboard
      "Mod+Shift+C".action = sh "env DISPLAY=:0 xsel -ob | wl-copy"; 
      "Mod+shift+V".action = sh "wl-paste -n | env DISPLAY=:0 xsel -ib";
      # media keys
      "XF86AudioRaiseVolume".action = sh "wpctl set-volume @DEFAULT_AUDIO_SINK@ 0.1+";
      "XF86AudioLowerVolume".action = sh "wpctl set-volume @DEFAULT_AUDIO_SINK@ 0.1-";
      "XF86AudioMute".action = sh "wpctl set-mute @DEFAULT_AUDIO_SINK@ toggle";
      "XF86MonBrightnessUp".action = sh "brightnessctl set 10%+";
      "XF86MonBrightnessDown".action = sh "brightnessctl set 10%-";
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
