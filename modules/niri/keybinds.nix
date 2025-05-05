{
  home.global = [
    ({config, ...}:
      with config.lib.niri.actions;
      with config.lib.stylix.colors.withHashtag; {
        binds = let
          mod = "Mod";
          sh = cmd: spawn "sh" "-c" "${cmd}";
        in {
          # programs
          "${mod}+Return".action = spawn "kitty";
          "${mod}+E".action = spawn "thunar";
          "${mod}+Space".action = sh ''
            wmenu-run -N "${base00}" -n "${base07}" -S "${base0D}" -s "${base00}"
          '';
          "${mod}+Shift+L".action = spawn "swaylock";
          "${mod}+W".action = sh ''systemctl --user restart waybar.service'';
          # clipboard
          "${mod}+Shift+C".action = sh "env DISPLAY=:0 xsel -ob | wl-copy";
          "${mod}+Shift+V".action = sh "wl-paste -n | env DISPLAY=:0 xsel -ib";
          # media keys
          "XF86AudioRaiseVolume".action = sh "wpctl set-volume @DEFAULT_AUDIO_SINK@ 0.05+";
          "XF86AudioLowerVolume".action = sh "wpctl set-volume @DEFAULT_AUDIO_SINK@ 0.05-";
          "XF86AudioMute".action = sh "wpctl set-mute @DEFAULT_AUDIO_SINK@ toggle";
          "XF86MonBrightnessUp".action = sh "brightnessctl set 5%+";
          "XF86MonBrightnessDown".action = sh "brightnessctl set 5%-";
          # screenshot
          "Print".action = screenshot;
          "Alt+Print".action = screenshot-window;
          # quits
          "${mod}+Q".action = close-window;
          "Ctrl+Alt+D".action = quit;
          "Ctrl+Shift+Alt+D".action = quit {skip-confirmation = true;};
          # window focus and move
          # "${modType}+UDLR".action = ${movement}-${node}-UDLR
          "${mod}+Up".action = focus-window-up;
          "${mod}+Down".action = focus-window-down;
          "${mod}+Left".action = focus-column-left;
          "${mod}+Right".action = focus-column-right;
          "${mod}+Shift+Up".action = move-window-up;
          "${mod}+Shift+Down".action = move-window-down;
          "${mod}+Shift+Left".action = move-column-left;
          "${mod}+Shift+Right".action = move-column-right;
          # workspace and monitor move
          "${mod}+Ctrl+up".action = focus-workspace-up;
          "${mod}+Ctrl+down".action = focus-workspace-down;
          "${mod}+Shift+Ctrl+up".action = move-window-to-workspace-up;
          "${mod}+Shift+Ctrl+down".action = move-window-to-workspace-down;
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
          "${mod}+Shift+Ctrl+Minus".action = set-column-width "-1%";
          "${mod}+Shift+Ctrl+Equal".action = set-column-width "+1%";
          # window presets
          "${mod}+R".action = switch-preset-column-width;
          "${mod}+M".action = expand-column-to-available-width;
          "${mod}+Ctrl+M".action = maximize-column;
          "${mod}+Shift+M".action = fullscreen-window;
          "${mod}+Period".action = consume-or-expel-window-right;
          "${mod}+Comma".action = consume-or-expel-window-left;
          # floating windows
          "${mod}+t".action = toggle-column-tabbed-display;
          "${mod}+f".action = switch-focus-between-floating-and-tiling;
          "${mod}+Shift+f".action = toggle-window-floating;
        };
      })
  ];
}
