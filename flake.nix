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

  outputs = { self, nixpkgs, ... } @ inputs: let
    system = "x86_64-linux";
    pkgs = import nixpkgs {inherit system;};
    wallpaper = pkgs.fetchurl {
      url = https://raw.githubusercontent.com/Chucklee1/nixos-dotfiles/refs/heads/main/Pictures/mono-forest.PNG;
      sha256 = "0clzh6jwi2ph7mjabzkh7aq1q9jzlhmzr6nr9q97jlf5a39js80s";
    };
    specialArgs = {inherit wallpaper inputs;};
    shared-modules = [
      ./modules/default.nix
      inputs.stylix.nixosModules.stylix
      inputs.niri.nixosModules.niri
      inputs.home-manager.nixosModules.home-manager
      inputs.grub2-themes.nixosModules.default
    ];
  in {
    # -----------------------------------------------------------
    # desktop profile
    # -----------------------------------------------------------
    nixosConfigurations.desktop = nixpkgs.lib.nixosSystem {
      system = system;
      specialArgs = specialArgs;
      modules = shared-modules ++ [
          ./modules/hardware/desktop.nix
          {
            vscode.enable = false;
            niri.enable = true;
            nvidia.enable = true;
            radeon.enable = false;
          }
        ];
    };
    # -----------------------------------------------------------
    # laptop profile
    # -----------------------------------------------------------
    nixosConfigurations.laptop = nixpkgs.lib.nixosSystem {
      system = system;
      specialArgs = specialArgs;
      modules = shared-modules ++ [
          ./modules/hardware/laptop.nix
          {
            vscode.enable = false;
            niri.enable = true;
            nvidia.enable = false;
            radeon.enable = true;
          }
        ];
    };
  };
}
# small notes:
# - default order parameters - { lib, config, pkgs, inputs, specialArgs, ... }
# # next to imports path = toggle module

