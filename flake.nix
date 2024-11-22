{
  description = "i dont kow what im doing";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    home-manager.url = "github:nix-community/home-manager";
    home-manager.inputs.nixpkgs.follows = "nixpkgs";
    stylix.url = "github:danth/stylix";
    niri.url = "github:sodiboo/niri-flake";
    grub2-themes.url = "github:vinceliuice/grub2-themes";
  };

  outputs = {
    self,
    nixpkgs,
    grub2-themes,
    ...
  } @ inputs: {
    # desktop profile
    nixosConfigurations.desktop = nixpkgs.lib.nixosSystem {
      system = "x86_64-linux";
      specialArgs = {inherit inputs;};
      modules = [
        # flake inputs
        inputs.home-manager.nixosModules.home-manager
        inputs.stylix.nixosModules.stylix
        inputs.niri.nixosModules.niri
        grub2-themes.nixosModules.default
        ./modules/default.nix
        ./modules/hardware/desktop.nix
        {
          nvidia.enable = true;
        }
      ];
    };
    # laptop profile
    nixosConfigurations.laptop = nixpkgs.lib.nixosSystem {
      system = "x86_64-linux";
      specialArgs = {inherit inputs;};
      modules = [
        # flake inputs
        inputs.home-manager.nixosModules.home-manager
        inputs.stylix.nixosModules.stylix
        inputs.niri.nixosModules.niri
        grub2-themes.nixosModules.default
        ./modules/default.nix
        ./modules/hardware/laptop.nix
        {
          nvidia.enable = false;
        }
      ];
    };
  };
}
# small notes:
# - default order parameters - { pkgs, inputs, lib, config, ... }

