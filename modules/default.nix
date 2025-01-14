{
  lib,
  config,
  pkgs,
  def,
  ...
}: {
  imports = [
    ./dwm.nix
    ./software.nix
    ./system.nix
    ./theming.nix
    ./hardware.nix
    inputs.home-manager.nixosModules.home-manager
    inputs.stylix.nixosModules.stylix
    inputs.grub2-themes.nixosModules.default
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
  home-manager.sharedModules = [inputs.nixvim.homeManagerModules.nixvim];
}
