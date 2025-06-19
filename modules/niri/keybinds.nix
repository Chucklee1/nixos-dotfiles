{
  home.global = [
    ({config, ...}:
      with config.lib.niri.actions;
      with config.lib.stylix.colors.withHashtag; {
        programs.niri.settings.binds = let
          mod = "Mod";
          sh = x: {action = spawn "sh" "-c" x;};
        in {
          # programs
          "${mod}+Return" = sh "kitty";
          "${mod}+E" = sh "kitty -e yazi";
          "${mod}+Shift+B" = sh "librewolf";
          "${mod}+Space" = sh ''wmenu-run -N "${base00}" -n "${base07}" -S "${base0D}" -s "${base00}"'';
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
          "Pause" = sh "rmpc togglepause";

          # screenshot
          "Print".action = screenshot;
          "Alt+Print".action = screenshot-window;
          # clipboard
          "${mod}+Shift+C" = sh "env DISPLAY=:0 xsel -ob | wl-copy";
          "${mod}+Shift+V" = sh "wl-paste -n | env DISPLAY=:0 xsel -ib";
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
}
