{
  lib,
  pkgs,
  def,
  ...
}: {
  # packages
  environment.systemPackages = with pkgs; [
    dmenu
    brightnessctl
    xclip
  ];

  services = {
    xserver = {
      enable = true;
      xkb.layout = "us";
      desktopManager.xterm.enable = false;
      windowManager.dwm = {
        enable = true;
        package = pkgs.dwm.overrideAttrs {
          src = builtins.fetchGit {
            url = "https://github.com/Chucklee1/dwm";
            rev = "a7a5994ea18123e14e245c016a8bec10e5596391";
          };
        };
        extraSessionCommands = ''
          export _JAVA_AWT_WM_NONREPARENTING=1
          ${lib.getExe pkgs.feh} --bg-scale ${def.wallpaper}
          ${lib.getExe pkgs.redshift} -m randr -O 5200
        '';
      };
    };
    picom = {
      enable = true;
      backend = "egl";
      vSync = true;
    };
  };

  # desktop portal
  xdg.portal = {
    enable = true;
    extraPortals = [pkgs.xdg-desktop-portal-gtk];
    config.common.default = ["gtk"];
  };
}
