{
  config,
  lib,
  pkgs,
  defaults,
  ...
}: {
  options.bspwm.enable = lib.mkEnableOption "enable bspwm";

  config = lib.mkIf config.bspwm.enable {
    services.xserver = {
      enable = true;
      windowManager.bspwm = {
        enable = true;
        configFile = builtins.toFile "bspwmrc" ''
          pgrep -x sxhkd > /dev/null || sxhkd &

          bspc monitor -d I II III IV V VI VII VIII IX X

          bspc config border_width         2
          bspc config window_gap          12

          bspc config split_ratio          0.52
          bspc config borderless_monocle   true
          bspc config gapless_monocle      true
        '';

        sxhkd.configFile = builtins.toFile "sxhkdrc" ''
          # programs
          super + Return
              ${defaults.terminal}

          super + space
              rofi -show drun

          super + e
            thunar

          # Media Keys
          XF86AudioRaiseVolume
              wpctl set-volume @DEFAULT_AUDIO_SINK@ 0.1+

          XF86AudioLowerVolume
              wpctl set-volume @DEFAULT_AUDIO_SINK@ 0.1-

          XF86AudioMute
              wpctl set-mute @DEFAULT_AUDIO_SINK@ toggle

          XF86AudioMicMute
              wpctl set-mute @DEFAULT_AUDIO_SOURCE@ toggle

          XF86MonBrightnessUp
              brightnessctl --device=amdgpu_bl1 s 5%+

          XF86MonBrightnessDown
              brightnessctl --device=amdgpu_bl1 s 5%-

          # Window actions
          super + q
              bspc node -c

          ctrl + alt + Delete
              bspwm-msg quit

          super + Left
              bspc focus left

          super + Right
              bspc focus right

          super + Up
              bspc focus up

          super + Down
              bspc focus down

          # Moving windows
          super + shift + Left
              bspc node -f left

          super + shift + Right
              bspc node -f right

          super + shift + Up
              bspc node -f up

          super + shift + Down
              bspc node -f down

          # Column manipulation
          super + comma
              bspc node -t tiled

          super + period
              bspc node -t floating

          # Column and Window size
          super + r
              bspc config border_width 1

          super + m
              bspc node -t full

          super + shift + m
              bspc node -t fullscreen

          super + minus
              bspc config border_width -10

          super + plus
              bspc config border_width +10

          super + shift + minus
              bspc node -z right -10 0

          super + shift + plus
              bspc node -z right +10 0
        '';
      };
    };
    home-manager.sharedModules = [
      {
        #services.polybar.enable = true;
        home.packages = with pkgs; [
          rofi # A more flexible alternative to dmenu
          feh # For setting background images
        ];
      }
    ];
  };
}
