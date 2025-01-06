{
  pkgs,
  lib,
  def,
  ...
}: {
  services.xserver = {
    enable = true;
    xkb.layout = def.layout;
    # dwm override
    windowManager.dwm = {
      enable = true;
      package = pkgs.dwm.overrideAttrs (old: {src = def.dwm-src;});
    };
    # startup commands
    displayManager = {
      xstart.enable = true;
      sessionCommands = ''
        ${lib.getExe pkgs.feh} --bg-scale $HOME/nixos-dotfiles/assets/wallpaper.png &
        ${lib.getExe pkgs.xorg.xrandr} --output DP-2 --mode 1920x1080 -r 165.00
        ${lib.getExe pkgs.redshift} -P -O 5100
      '';
    };
  };
}
