{pkgs, ...}: {
  home-manager.sharedModules = [
    {
      programs.niri.settings = {
        # general
        prefer-no-csd = true;
        hotkey-overlay.skip-at-startup = true;
        screenshot-path = "~/Pictures/Screenshots/Screenshot from %Y-%m-%d %H-%M-%S.png";
        # startup
        spawn-at-startup = [
          {command = ["waybar"];}
          {command = ["nm-applet"];}
          {command = ["lxqt-policykit-agent"];}
          {command = ["wlsunset -T 5200"];}
          {command = ["swww-daemon && swww img $HOME/nixos-dotfiles/assets/wallpaper.png"];}
        ];
        # keybinds
        binds = let
          spawn = command: {action.spawn = ["sh" "-c" ''${command}''];};
          action = command: {action.spawn = ["sh" "-c" ''niri msg action ${command}''];};
        in {
          # programs
          "Mod+Return" = spawn "kitty -e tmux";
          "Mod+E" = spawn "thunar";
          "Mod+Space" = spawn "fuzzel";
          "Super+Shift+L" = spawn "swaylock";
          "Super+Shift+P" = spawn "wlogout";
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
          # window presets
          "Mod+R" = action "switch-preset-column-width";
          "Mod+M" = action "maximize-column";
          "Mod+Shift+M" = action "fullscreen-window";
        };
        # input
        input = {
          keyboard.xkb.layout = "us";
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
        outputs."HKC OVERSEAS LIMITED 24E4 0000000000001" = {
          enable = true;
          mode.width = 1920;
          mode.height = 1080;
          position.x = 0;
          position.y = 0;
          mode.refresh = 165.001;
        };
        outputs."eDP-1" = {
          enable = true;
          mode.width = 1920;
          mode.height = 1080;
          position.x = 0;
          position.y = 0;
          mode.refresh = 60.008;
        };
        # layout
        layout = {
          gaps = 0;
          border.width = 2;
          always-center-single-column = false;
        };
      };
    }
  ];
}
