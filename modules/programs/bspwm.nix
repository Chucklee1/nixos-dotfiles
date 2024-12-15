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
      picom
      redshift
      dunst
    ];
    services.xserver = {
      enable = true;
      xkb.layout = "us";
      windowManager.bspwm.enable = true;
    };
    home-manager.sharedModules = [
      {
        services.sxhkd = {
          enable = true;
          keybindings = {
            # hotkeys
            "super + return" = "kitty -e tmux";
            "super + e" = "thunar";
            "super + space" = "rofi -show drun";
            "ctrl + alt + f" = "firefox";

            # windows
            "shift + up" = "bspc node -f north";
            "shift + down" = "bspc node -f south";
            "Shift + left" = "bspc node -f east";
            "shift + right" = "bspc node -f west";

            # layouts
            "super + t" = "bspc desktop -l tiled";
            "super + m" = "bspc desktop -l monocle";
            "super + f" = "bspc desktop -l floating";
          };
        };
        xsession.windowManager.bspwm = {
          startupPrograms = [
            "picom"
            "redshift"
            "dunst"
            "feh --bg-scale /home/goat/nixos-dotfiles/assets/wallpaper.PNG"
          ];
          monitors.DP-1 = ["1" "2" "3" "4"];
          settings = {
            focus_follows_pointer = true;
            border_width = 2;
            window_gap = 12;
            split_ratio = 0.5;
            borderless_monocle = true;
            gapless_monocle = true;
          };
        };
      }
    ];
  };
}
