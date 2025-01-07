{
  config,
  lib,
  pkgs,
  def,
  ...
}: {
  programs = {
    fuzzel.enable = true;
    wlogout.enable = true;
    swaylock = {
      enable = true;
      package = pkgs.swaylock-effects;
    };
    niri.settings = {
      # general
      prefer-no-csd = true;
      hotkey-overlay.skip-at-startup = true;
      screenshot-path = "~/Pictures/Screenshots/Screenshot from %Y-%m-%d %H-%M-%S.png";
      # env vars
      environment = {
        # nvidia
        GBM_BACKEND = "nvidia-drm";
        NVD_BACKEND = "direct";
        WLR_NO_HARDWARE_CURSORS = "1";
        WLR_RENDERER_ALLOW_SOFTWARE = "1";
        ELECTRON_OZONE_PLATFORM_HINT = "auto";
        # the rest
        XDG_CURRENT_DESKTOP = "niri";
        XDG_SESSION_DESKTOP = "niri";
        NIXOS_OZONE_WL = "1";
        MOZ_ENABLE_WAYLAND = "1";
        SDL_VIDEODRIVER = "wayland,x11";
        GDK_BACKEND = "wayland,x11";
        QT_QPA_PLATFORM = "wayland;xcb";
        QT_WAYLAND_DISABLE_WINDOWDECORATION = "1";
        QT_AUTO_SCREEN_SCALE_FACTOR = "1";
        DISPLAY = ":0";
      };

      spawn-at-startup = let
        sh = cmd: ["sh" "-c" ''${cmd}''];
      in [
        {
          command = [
            "${lib.getExe pkgs.networkmanagerapplet}"
          ];
        }
        {
          command = [
            "${lib.getExe pkgs.xwayland-satellite}"
          ];
        }
        {
          command = [
            "${lib.getExe pkgs.wlsunset}"
            "-T"
            "5200"
          ];
        }
        {
          command = [
            "${lib.getExe pkgs.swaybg}"
            "-i"
            "${def.wallpaper}"
            "-m"
            "fill"
          ];
        }
        {
          command = sh ''
            if systemctl --user list-units --type=service --all | grep -q 'waybar.service'; then
              systemctl --user restart waybar.service
            fi
          '';
        }
      ];

      # keybinds
      binds = with config.lib.niri.actions; let
        sh = spawn "sh" "-c";
        mod = "Mod";
        mod-s = "Mod+Shift";
        mod-c = "Mod+Ctrl";
        mod-s-c = "Mod+Shift+Ctrl";
      in {
        # hotkeys - programs
        "${mod}+Return".action = spawn "kitty";
        "${mod}+E".action = spawn "thunar";
        "${mod}+Space".action = spawn "fuzzel";
        "${mod-s}+L".action = spawn "swaylock";
        "${mod-s}+P".action = spawn "wlogout";
        "${mod}+W".action = sh ''systemctl --user restart waybar.service'';
        # hotkeys - clipboard
        "${mod-s}+C".action = sh "env DISPLAY=:0 xsel -ob | wl-copy";
        "${mod-s}+V".action = sh "wl-paste -n | env DISPLAY=:0 xsel -ib";
        # hotkeys - media keys
        "XF86AudioRaiseVolume".action = sh "wpctl set-volume @DEFAULT_AUDIO_SINK@ 0.05+";
        "XF86AudioLowerVolume".action = sh "wpctl set-volume @DEFAULT_AUDIO_SINK@ 0.05-";
        "XF86AudioMute".action = sh "wpctl set-mute @DEFAULT_AUDIO_SINK@ toggle";
        "XF86MonBrightnessUp".action = sh "${lib.getExe pkgs.brightnessctl} set 5%+";
        "XF86MonBrightnessDown".action = sh "${lib.getExe pkgs.brightnessctl} set 5%-";
        # niri - screenshot
        "Print".action = screenshot;
        "Ctrl+Print".action = screenshot-screen;
        "Alt+Print".action = screenshot-window;
        # niri - quits
        "${mod}+Q".action = close-window;
        "Ctrl+Alt+Delete".action = quit;
        # niri - window focus and move
        "${mod}+Up".action = focus-window-up;
        "${mod}+Down".action = focus-window-down;
        "${mod}+Left".action = focus-column-left;
        "${mod}+Right".action = focus-column-right;
        "${mod-s}+Up".action = move-window-up;
        "${mod-s}+Down".action = move-window-down;
        "${mod-s}+Left".action = move-column-left;
        "${mod-s}+Right".action = move-column-right;
        # niri - window presets
        "${mod}+R".action = switch-preset-column-width;
        "${mod}+M".action = maximize-column;
        "${mod-s}+M".action = fullscreen-window;
        "${mod}+Period".action = consume-or-expel-window-right;
        "${mod}+Comma".action = consume-or-expel-window-left;
        # niri - column width - using = since + needs shift
        "${mod}+Minus".action = set-column-width "-10%";
        "${mod}+Equal".action = set-column-width "+10%";
        "${mod-s}+Minus".action = set-column-width "-1%";
        "${mod-s}+Equal".action = set-column-width "+1%";
        "${mod-c}+Minus".action = set-window-height "-10%";
        "${mod-c}+Equal".action = set-window-height "+10%";
        "${mod-s-c}+Minus".action = set-column-width "-1%";
        "${mod-s-c}+Equal".action = set-column-width "+1%";
        # niri - floating windows
        "${mod}+f".action = switch-focus-between-floating-and-tiling;
        "${mod-s}+f".action = toggle-window-floating;
        # debugging
        "Ctrl+Shift+d".action = toggle-debug-tint;
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
      };
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
        gaps = 7;
        border.width = 2;
        always-center-single-column = false;
      };
      window-rules = [
        {
          geometry-corner-radius = let
            r = 4.0;
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
  };
}
