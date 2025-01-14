{
  pkgs,
  inputs,
  def,
  ...
}: let
  file = import ./hardware.nix;

  hostToMod = {
    laptop = "nix-laptop";
    desktop = "nix-desktop";
  };

  config =
    if hostToMod ? def.host
    then file.${hostToMod}.${def.host}
    else {};
in
  {
    imports = [
      ./software.nix
      ./system.nix
      ./theming.nix
      ./hosts/${def.host}.nix
    ];

    home-manager = {
      useUserPackages = true;
      useGlobalPkgs = true;
      extraSpecialArgs = {inherit inputs def;};
      users.${def.username}.home = {
        stateVersion = "24.05"; # DO NOT CHANGE
        username = "${def.username}";
        homeDirectory = "/home/${def.username}";
      };
    };
  }
  // config
