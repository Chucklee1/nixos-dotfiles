{
  pkgs,
  inputs,
  ...
}: {
  nixpkgs.overlays = [inputs.niri.overlays.niri];
  programs.niri = {
    enable = true;
    package = pkgs.niri-unstable;
  };
  environment.systemPackages = with pkgs; [
    egl-wayland
    qt5.qtwayland
    qt6.qtwayland
    wev
    xwayland
    xwayland-run
    wl-clipboard
  ];
  xdg.portal = {
    enable = true;
    extraPortals = [
      pkgs.xdg-desktop-portal-gtk
      pkgs.xdg-desktop-portal-gnome
    ];
    config.common.default = "*";
  };

  home-manager.sharedModules = [
    { 
      programs = {
        fuzzel.enable = true;
        wlogout.enable = true;
        swaylock = {
          enable = true;
          package = pkgs.swaylock-effects;
        };
        waybar = {
          enable = true;
          systemd.enable = true;
        };
      };
    }
  ];
}
