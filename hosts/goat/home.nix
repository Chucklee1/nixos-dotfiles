{
  pkgs,
  lib,
  config,
  ...
}: {
  imports = [../../modules/home/default.nix];
  home.username = "goat";
  home.homeDirectory = "/home/goat";
  home.stateVersion = "24.05"; # D O  N O T  C H A N G E
}
