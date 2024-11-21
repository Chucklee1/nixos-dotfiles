{
  pkgs,
  inputs,
  lib,
  config,
  ...
}: {
  options = {niri.enable = lib.mkEnableOption "enable niri window manager module";};

  config = lib.mkIf config.niri.enable {
    # package extension
    nixpkgs.overlays = [inputs.niri.overlays.niri];
    programs.niri = {
      enable = true;
      package = pkgs.niri-unstable;
    };
    # home manager
    home-manager.users.goat = {
      # nested niri.settings so config.lib.niri.actions will work
      imports = [./settings.nix];
      stylix.targets.niri.enable = true;
    };
    # gtk portal
    xdg.portal = {
      extraPortals = pkgs.xdg-desktop-portal-gtk;
      configPackages = pkgs.xdg-desktop-portal-gtk;
    };
  };
}
