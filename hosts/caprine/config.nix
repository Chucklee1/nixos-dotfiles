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
  nvidia.enable = false;

  # user
  networking.hostName = "caprine";
  users.users.caprine = {
    isNormalUser = true;
    extraGroups = ["wheel" "networkmanager"];
  };

  # home manager
  home-manager = {
    users.caprine = {
      imports = [../../modules/home/default.nix];
      home.username = "caprine";
      home.homeDirectory = "/home/caprine";
      home.stateVersion = "24.05"; # D O  N O T  C H A N G E
    };
  };

  # ================================================================ #
  # =                         DO NOT TOUCH                         = #
  # ================================================================ #
  system.stateVersion = "24.05"; # D O  N O T  C H A N G E
}
