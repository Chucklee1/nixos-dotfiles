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

  # user
  networking.hostName = "goat";
  users.users.goat = {
    isNormalUser = true;
    extraGroups = ["wheel" "networkmanager"];
  };

  # home manager
  home-manager = {
    users.goat = {
      imports = [
        ../../modules/home/default.nix
        ../../modules/home/waybar-desktop.nix
      ];
      home.username = "goat";
      home.homeDirectory = "/home/goat";
      home.stateVersion = "24.05"; # D O  N O T  C H A N G E
    };
  };

  # ================================================================ #
  # =                         DO NOT TOUCH                         = #
  # ================================================================ #
  system.stateVersion = "24.05"; # D O  N O T  C H A N G E
}
