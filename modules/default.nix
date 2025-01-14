{
  pkgs,
  inputs,
  host,
  def,
  ...
}: let
  file = import ./hardware.nix;

  hostToMod = {
    laptop = "nix-laptop";
    desktop = "nix-desktop";
  };

  config =
    if hostToMod ? host
    then file.${hostToMod}.${host}
    else {};
in
  {
    imports = [
      ./software.nix
      ./system.nix
      ./theming.nix
      ./hosts/${host}.nix
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
