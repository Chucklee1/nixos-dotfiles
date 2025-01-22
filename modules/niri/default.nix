{
  config,
  lib,
  pkgs,
  inputs,
  def,
  ...
}: {
  imports = [inputs.niri.nixosModules.niri];

  nixpkgs.overlays = [inputs.niri.overlays.niri];
  programs.niri = {
    enable = true;
    package = pkgs.niri-unstable;
  };
  environment.systemPackages = with pkgs; [
    egl-wayland
    qt5.qtwayland
    qt6.qtwayland
    brightnessctl
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
        wpaperd.enable = true;
        swaylock = {
          enable = true;
          package = pkgs.swaylock-effects;
        };
        waybar = with (def.files {inherit config lib;}); {
          enable = true;
          systemd.enable = true;
          settings = waybarConfig;
          style = waybarStyle;
        };
      };
    }
  ];
}
