{
  description = "i dont kow what im doing";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    home-manager.url = "github:nix-community/home-manager";
    home-manager.inputs.nixpkgs.follows = "nixpkgs";
    stylix.url = "github:danth/stylix";
    niri.url = "github:sodiboo/niri-flake";
    nixvim.url = "github:nix-community/nixvim";
    nixvim.inputs.nixpkgs.follows = "nixpkgs";
    grub2-themes.url = "github:vinceliuice/grub2-themes";
  };

  outputs = {nixpkgs, ...} @ inputs: let
    systemConfig = machine:
      nixpkgs.lib.nixosSystem {
        specialArgs = let
          def = rec {
            username = "goat";
            host = machine;
            hostname = "${machine}-${username}";
            system = "x86_64-linux";
            wallpaper = ./assets/wallpaper.png;
          };
        in {
          inherit inputs def;
        };
        modules = [
          ./modules/default.nix
          inputs.home-manager.nixosModules.home-manager
          inputs.stylix.nixosModules.stylix
          inputs.grub2-themes.nixosModules.default
          inputs.niri.nixosModules.niri
          {home-manager.sharedModules = [inputs.nixvim.homeManagerModules.nixvim];}
        ];
      };
  in {
    # systems
    nixosConfigurations.desktop = systemConfig "desktop";
    nixosConfigurations.laptop = systemConfig "laptop";
  };
}
