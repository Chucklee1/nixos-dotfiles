let
  homeNiri = [
    ({config, ...}:
      with config.lib.niri.actions;
      with config.lib.stylix.colors.withHashtag; {
        programs.niri.settings.binds = let
          # helpers
          sh = x: {action = spawn "sh" "-c" x;};
          cmd = x: {action = x;};
          # defaults
          mod = "Mod";
          terminal = "kitty";
          browser = "librewolf";
          file-manager = "${terminal} -e yazi";
          app-launcher = ''wmenu-run -N "${base00}" -n "${base07}" -S "${base0D}" -s "${base00}"'';
        in {
          # ---- niri-external ----
          # programs
          "${mod}+Return" = sh terminal;
          "${mod}+E" = sh file-manager;
          "${mod}+Shift+B" = sh browser;
          "${mod}+Space" = sh app-launcher;
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
          "KP_Begin" = sh "rmpc togglepause";
          "KP_Right" = sh "rmpc next";
          "KP_Left" = sh "rmpc prev";
          # clipboard
          "${mod}+Shift+C" = sh "env DISPLAY=:0 xsel -ob | wl-copy";
          "${mod}+Shift+V" = sh "wl-paste -n | env DISPLAY=:0 xsel -ib";

          # ---- niri-internal ----
          # screenshot
          "Print" = cmd screenshot;
          "Alt+Print" = cmd screenshot-window;
          # quits
          "${mod}+Q" = cmd close-window;
          "Ctrl+Alt+Delete" = cmd quit;
          "Ctrl+Shift+Alt+Delete" = cmd quit {skip-confirmation = true;};

          # moving
          "${mod}+Up" = cmd focus-window-or-workspace-up;
          "${mod}+Down" = cmd focus-window-or-workspace-down;
          "${mod}+Shift+Up" = cmd move-window-up-or-to-workspace-up;
          "${mod}+Shift+Down" = cmd move-window-down-or-to-workspace-down;
          "${mod}+Left" = cmd focus-column-left;
          "${mod}+Right" = cmd focus-column-right;
          "${mod}+Shift+Left" = cmd move-column-left;
          "${mod}+Shift+Right" = cmd move-column-right;
          "${mod}+Alt+left" = cmd focus-monitor-next;
          "${mod}+Alt+right" = cmd focus-monitor-previous;
          "${mod}+Shift+Alt+left" = cmd move-window-to-monitor-next;
          "${mod}+Shift+Alt+right" = cmd move-window-to-monitor-previous;

          # column width - using = since + needs shift
          "${mod}+Minus" = cmd set-column-width "-10%";
          "${mod}+Equal" = cmd set-column-width "+10%";
          "${mod}+Shift+Minus" = cmd set-column-width "-1%";
          "${mod}+Shift+Equal" = cmd set-column-width "+1%";
          "${mod}+Ctrl+Minus" = cmd set-window-height "-10%";
          "${mod}+Ctrl+Equal" = cmd set-window-height "+10%";
          # window presets
          "${mod}+R" = cmd switch-preset-column-width;
          "${mod}+M" = cmd expand-column-to-available-width;
          "${mod}+Ctrl+M" = cmd maximize-column;
          "${mod}+Shift+M" = cmd fullscreen-window;
          "${mod}+Period" = cmd consume-or-expel-window-right;
          "${mod}+Comma" = cmd consume-or-expel-window-left;
          # layouts
          "${mod}+t" = cmd toggle-column-tabbed-display;
          "${mod}+f" = cmd switch-focus-between-floating-and-tiling;
          "${mod}+Shift+f" = cmd toggle-window-floating;
        };
      })
  ];
in {
  home.desktop = homeNiri;
}
