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
  } @ inputs: let
    wallpaper = /home/goat/Pictures/mono-forest.PNG;
    specialArgs = {inherit wallpaper inputs;};
    shared-modules = [
      ./modules/default.nix
      inputs.home-manager.nixosModules.home-manager
      inputs.stylix.nixosModules.stylix
      inputs.niri.nixosModules.niri
      grub2-themes.nixosModules.default
    ];
  in {
    # -----------------------------------------------------------
    # desktop profile
    # -----------------------------------------------------------
    nixosConfigurations.desktop = nixpkgs.lib.nixosSystem {
      system = "x86_64-linux";
      specialArgs = specialArgs;
      modules =
        shared-modules
        ++ [
          ./modules/hardware/desktop.nix
          {
            vscode.enable = true;
            waybar.enable = true;
            #niri.enable = true;
            nvidia.enable = true;
            #niri-nvidia.enable = true;
            radeon.enable = false;
          }
        ];
    };
    # -----------------------------------------------------------
    # laptop profile
    # -----------------------------------------------------------
    nixosConfigurations.laptop = nixpkgs.lib.nixosSystem {
      system = "x86_64-linux";
      specialArgs = specialArgs;
      modules =
        shared-modules
        ++ [
          ./modules/hardware/laptop.nix
          {
            vscode.enable = true;
            waybar.enable = true;
            #niri.enable = true;
            nvidia.enable = false;
            #niri-nvidia.enable = false;
            radeon.enable = true;
          }
        ];
    };
  };
}
# small notes:
# - default order parameters - { pkgs, inputs, lib, config, ... }
# # next to imports path = toggle module

