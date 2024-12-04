{
  pkgs,
  inputs,
  lib,
  config,
  ...
}: {
  options = {
    niri.enable = lib.mkEnableOption "enable nvidia progam";
  };

  config = lib.mkIf config.niri.enable {
    nixpkgs.overlays = [inputs.niri.overlays.niri];
    programs = {
      niri.enable = true;
      niri.package = pkgs.niri-unstable;
    };
    home-manager.users.goat.imports = [../config/config.kdl.nix];
  };
}
