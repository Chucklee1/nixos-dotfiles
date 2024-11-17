{
  pkgs,
  inputs,
  ...
}: {
  imports = [inputs.niri.nixosModules.niri];
  # -----------------------------------------------------------
  # niri setup ( wont work in home manager idk why )
  # -----------------------------------------------------------
  nixpkgs.overlays = [inputs.niri.overlays.niri];
  programs.niri = {
    enable = true;
    package = pkgs.niri-unstable; # make niri use overlay poackage
  };
  # nested niri.settings so config.lib.niri.actions will work
  home-manager.users.goat.imports = [./niri-settings.nix];
}
