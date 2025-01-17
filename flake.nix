{
  description = "i dont kow what im doing";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    home-manager.url = "github:nix-community/home-manager";
    home-manager.inputs.nixpkgs.follows = "nixpkgs";
    stylix.url = "github:danth/stylix";
    #niri.url = "github:sodiboo/niri-flake";
    nixvim.url = "github:nix-community/nixvim";
    nixvim.inputs.nixpkgs.follows = "nixpkgs";
    grub2-themes.url = "github:vinceliuice/grub2-themes";
  };

  outputs = {nixpkgs, ...} @ inputs: let
    mkSystem = host:
      nixpkgs.lib.nixosSystem rec {
        system = "x86_64-linux";
        specialArgs = {
          inherit system inputs;
          def = {
            username = "goat";
            inherit host;
            wallpaper = ./assets/wallpaper.png;
          };
        };
        modules = [
          ./modules/dwm.nix
          ./modules/hardware.nix
          ./modules/software.nix
          ./modules/system.nix
          ./modules/theming.nix
          ./modules/hosts/${host}.nix
          inputs.stylix.nixosModules.stylix
          #inputs.niri.nixosModules.niri
          inputs.grub2-themes.nixosModules.default
          inputs.home-manager.nixosModules.home-manager
          {
            home-manager.sharedModules = [
              inputs.nixvim.homeManagerModules.nixvim
              {programs.nixvim = (import ./modules/nixvim.nix);}
            ];
          }
        ];
      };
  in {
    # mkSystem declarations
    nixosConfigurations.desktop = mkSystem "desktop";
    nixosConfigurations.laptop = mkSystem "laptop";
  };
}
