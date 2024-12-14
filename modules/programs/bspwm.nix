{
  config,
  lib,
  pkgs,
  defaults,
  ...
}: {
  options.bspwm.enable = lib.mkEnableOption "enable bspwm";

  config = lib.mkIf config.bspwm.enable {
    environment.systemPackages = with pkgs; [
      xorg.xev
      rofi
      feh
    ];
    services.xserver = {
      enable = true;
      windowManager.bspwm = {
        enable = true;
        configFile = builtins.toFile "bspwmrc" ''
          pgrep -x sxhkd > /dev/null || sxhkd &

          bspc monitor -d I II III IV V VI VII VIII IX X

          bspc config border_width         2
          bspc config window_gap          5

          bspc config split_ratio          0.5
          bspc config borderless_monocle   true
          bspc config gapless_monocle      true
        '';

        sxhkd.configFile = builtins.toFile "sxhkdrc" ''
          super + Return
            kitty

          super + space
            rofi -mode drun -show drun

          super + e
            thunar

          XF86MonBrightness{Down,Up}
            light {-U 5,-A 5}

          XF86Audio{LowerVolume,RaiseVolume}
            pactl set-sink-volume 1 {-1%,+1%}

          XF86AudioMute
            pactl set-sink-mute 1 toggle

          super + {t,shift + t,s,m}
            bspc node -t {tiled,pseudo_tiled,floating,fullscreen}

          super + q
            bspc node -c

          ctrl + alt + Delete
            bsc quit

          super + {_,shift + }{Up,Down,Left,Right}
            bspc node -{f,s} {north,south,west,east}
        '';
      };
    };
  };
}
