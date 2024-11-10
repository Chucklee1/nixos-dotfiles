{
  pkgs,
  lib,
  config,
  ...
}: {
  imports = [
    ./hardware-configuration.nix
    ../../modules/default.nix
  ];

  # modules
  nvidia.enable = true;
  radeon.enable = false;

  # user
  networking.hostName = "goat";
  users.users.goat = {
    isNormalUser = true;
    extraGroups = ["wheel" "networkmanager"];
  };

  # ================================================================ #
  # =                         DO NOT TOUCH                         = #
  # ================================================================ #
  system.stateVersion = "24.05"; # D O  N O T  C H A N G E
}
