{inputs, ...}: {
  wayland = {
    nix = [
      inputs.niri.nixosModules.niri
      ({pkgs, ...}: {
        nixpkgs.overlays = [inputs.niri.overlays.niri];
        programs.niri.package = pkgs.niri-unstable;
        programs.niri.enable = true;
      })
    ];
    home = [
      ({
        lib,
        config,
        pkgs,
        machine,
        ...
      }: {
        programs.niri.settings = {
          # general
          hotkey-overlay.skip-at-startup = machine != "umbra";
          prefer-no-csd = true;
          screenshot-path = "~/Pictures/Screenshots/Screenshot from %Y-%m-%d %H-%M-%S.png";
          environment = {
            NIXOS_OZONE_WL = "1";
            MOZ_ENABLE_WAYLAND = "1";
            DISPLAY = ":0";
            _JAVA_AWT_WM_NONREPARENTING = "1";
            SDL_VIDEODRIVER = "x11";
            GDK_BACKEND = "wayland,x11";
            QT_QPA_PLATFORM = "wayland;xcb";
            QT_WAYLAND_DISABLE_WINDOWDECORATION = "1";
            QT_AUTO_SCREEN_SCALE_FACTOR = "1";
          };
          spawn-at-startup = let
            get = pkg: lib.getExe pkgs.${pkg};
          in [
            {command = ["${get "xwayland-satellite"}"];}
            {command = ["${get "wlsunset"}" "-T" "5200"];}
            {command = ["${get "swaybg"}" "-m" "fill" "-i" "${config.stylix.image}"];}
            {command = ["brightnessctl" "s" "50%"];}
            {command = ["systemctl" "--user" "reset-failed" "waybar.service"];}
            {command = ["wpctl" "set-mute" "@DEFAULT_AUDIO_SINK@" "1"];}
          ];
          # input
          input = {
            keyboard = {
              xkb.options = "ctrl:nocaps";
              numlock = true;
            };
            mouse.accel-speed = 0.0;
            tablet.map-to-output = "eDP-1";
            touch.map-to-output = "eDP-1";
            touchpad = {
              tap = true;
              dwt = true;
              natural-scroll = true;
              click-method = "clickfinger";
            };
          };
          cursor.hide-after-inactive-ms = 5000;
          # layout n theming
          layout = {
            gaps = 0;
            border.width = 2;
            always-center-single-column = false;
          };
          # disable annoying hot-corners
          gestures.hot-corners.enable = false;
          window-rules = let
            r = 1.0;
          in [
            {
              geometry-corner-radius = {
                top-left = r;
                top-right = r;
                bottom-left = r;
                bottom-right = r;
              };
              clip-to-geometry = true;
            }
            {
              matches = [{app-id = "^org.prismlauncher.PrismLauncher$";}];
              open-floating = false;
            }
            {
              matches = [{app-id = "org.kde.polkit-kde-authentication-agent$";}];
              block-out-from = "screen-capture";
              open-floating = true;
            }
          ];
        };
      })
      # keybinds
      ({config, ...}:
        with config.lib.niri.actions;
        with config.lib.stylix.colors.withHashtag; {
          programs.niri.settings.binds = let
            # helpers
            sh = x: {action = spawn "sh" "-c" x;};
            wmenu = ''
              wmenu-run -N \
              "${base00}" \
              -n "${base07}" \
              -S "${base0D}" \
              -s "${base00}"
            '';
            # mod def
            mod = "Mod";
          in {
            # programs
            "${mod}+Return" = sh "kitty";
            "${mod}+E" = sh "emacs";
            "${mod}+Shift+B" = sh "librewolf";
            "${mod}+Space" = sh wmenu;
            "${mod}+Shift+L" = sh "swaylock";
            "${mod}+W" = sh ''systemctl --user restart waybar.service'';

            # media keys
            "XF86AudioRaiseVolume" = sh "wpctl set-volume @DEFAULT_AUDIO_SINK@ 0.05+";
            "XF86AudioLowerVolume" = sh "wpctl set-volume @DEFAULT_AUDIO_SINK@ 0.05-";
            "XF86AudioMute" = sh "wpctl set-mute @DEFAULT_AUDIO_SINK@ toggle";
            "XF86MonBrightnessUp" = sh "brightnessctl set 5%+";
            "XF86MonBrightnessDown" = sh "brightnessctl set 5%-";
            "XF86KbdBrightnessUp" = sh "brightnessctl --device=smc::kbd_backlight set 10%+";
            "XF86KbdBrightnessDown" = sh "brightnessctl --device=smc::kbd_backlight set 10%-";
            # rmpc
            "XF86AudioPlay" = sh "rmpc togglepause";
            "XF86AudioNext" = sh "rmpc next";
            "XF86AudioPrev" = sh "rmpc prev";
            # clipboard
            "${mod}+Shift+C" = sh "env DISPLAY=:0 xsel -ob | wl-copy";
            "${mod}+Shift+V" = sh "wl-paste -n | env DISPLAY=:0 xsel -ib";
            # screenshot
            "Print".action = screenshot;
            "Alt+Print".action = screenshot-window;
            # quits
            "${mod}+Q".action = close-window;
            "Ctrl+Alt+Delete".action = quit;
            "Ctrl+Shift+Alt+Delete".action = quit {skip-confirmation = true;};

            # moving
            "${mod}+Up".action = focus-window-or-workspace-up;
            "${mod}+Down".action = focus-window-or-workspace-down;
            "${mod}+Shift+Up".action = move-window-up-or-to-workspace-up;
            "${mod}+Shift+Down".action = move-window-down-or-to-workspace-down;
            "${mod}+Left".action = focus-column-left;
            "${mod}+Right".action = focus-column-right;
            "${mod}+Shift+Left".action = move-column-left;
            "${mod}+Shift+Right".action = move-column-right;
            "${mod}+Alt+left".action = focus-monitor-next;
            "${mod}+Alt+right".action = focus-monitor-previous;
            "${mod}+Shift+Alt+left".action = move-window-to-monitor-next;
            "${mod}+Shift+Alt+right".action = move-window-to-monitor-previous;

            # column width - using = since + needs shift
            "${mod}+Minus".action = set-column-width "-10%";
            "${mod}+Equal".action = set-column-width "+10%";
            "${mod}+Shift+Minus".action = set-column-width "-1%";
            "${mod}+Shift+Equal".action = set-column-width "+1%";
            "${mod}+Ctrl+Minus".action = set-window-height "-10%";
            "${mod}+Ctrl+Equal".action = set-window-height "+10%";
            # window presets
            "${mod}+R".action = switch-preset-column-width;
            "${mod}+M".action = expand-column-to-available-width;
            "${mod}+Ctrl+M".action = maximize-column;
            "${mod}+Shift+M".action = fullscreen-window;
            "${mod}+Period".action = consume-or-expel-window-right;
            "${mod}+Comma".action = consume-or-expel-window-left;
            # layouts
            "${mod}+t".action = toggle-column-tabbed-display;
            "${mod}+f".action = switch-focus-between-floating-and-tiling;
            "${mod}+Shift+f".action = toggle-window-floating;
          };
        })
    ];
  };
  desktop.home = [
    ({lib, ...}: {
      programs.niri.settings = {
        input.keyboard.xkb.options = lib.mkForce "ctrl:nocaps,altwin:swap_alt_win";
        outputs."DP-1".mode = {
          width = 1920;
          height = 1080;
          refresh = 165.001;
        };
      };
    })
  ];
}
