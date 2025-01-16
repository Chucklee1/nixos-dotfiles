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

  outputs = {nixpkgs, ...} @ inputs: rec {
    system = "x86_64-linux";
    pkgs = import nixpkgs {inherit system;};
    lib = pkgs.lib;

    mkSystem = host:
      lib.nixosSystem {
        specialArgs = {
          inherit system inputs;
          def = {
            username = "goat";
            machine = host;
            wallpaper = ./assets/wallpaper.png;
          };
        };

        modules = [
          ./dwm.nix
          ./hardware.nix
          #./niri.nix
          ./software.nix
          ./system.nix
          ./theming.nix
          inputs.stylix.nixosModules.stylix
          inputs.niri.nixosModules.niri
          inputs.grub2-themes.nixosModules.default
          inputs.home-manager.nixosModules.home-manager
          {
            home-manager = {
              sharedModules = [inputs.nixvim.homeManagerModules.nixvim];
            };
          }
        ];
      };

    # mkSystem declarations
    nixosConfigurations.desktop = mkSystem "desktop";
    nixosConfigurations.laptop = mkSystem "laptop";
  };
}
