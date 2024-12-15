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
      windowManager.bspwm = {
        enable = true;
        configFile = "/home/goat/nixos-dotfiles/assets/bspwmrc";

        sxhkd.configFile = "/home/goat/nixos-dotfiles/assets/sxhkdrc";
      };
    };
  };
}
