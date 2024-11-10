{ pkgs, lib, config, ... }:
  {
  imports = [ 
    ./hardware-configuration.nix
    ../../modules/default.nix 
  ]; 
  
  # modules
  nvidia.enable = false;
  radeon.enable = false;

  # user
  networking.hostName = "caprine";
  users.users.caprine = {
    isNormalUser = true;
    extraGroups = [ "wheel" "networkmanager" ];  
  };

  # ================================================================ #
  # =                         DO NOT TOUCH                         = #
  # ================================================================ #
  system.stateVersion = "24.05"; # D O  N O T  C H A N G E
}