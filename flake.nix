{
  description = "i dont kow what im doing";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    home-manager.url = "github:nix-community/home-manager";
    home-manager.inputs.nixpkgs.follows = "nixpkgs";
    stylix.url = "github:danth/stylix";
    nixvim.url = "github:nix-community/nixvim";
    nixvim.inputs.nixpkgs.follows = "nixpkgs";
    grub2-themes.url = "github:vinceliuice/grub2-themes";
  };

  outputs = {nixpkgs, ...} @ inputs: let
    systemConfig = host:
      nixpkgs.lib.nixosSystem {
        specialArgs = {
          inherit inputs;
          def = rec {
            username = "goat";
            hostname = "${host}-${username}";
            system = "x86_64-linux";
            wallpaper = /home/goat/nixos-dotfiles/assets/wallpaper.png;
          };
        };
        modules = let
          files = [
            /home/goat/nixos-dotfiles/modules/dwm.mod.nix
            /home/goat/nixos-dotfiles/modules/hardware.mod.nix
            /home/goat/nixos-dotfiles/modules/software.mod.nix
            /home/goat/nixos-dotfiles/modules/system.mod.nix
            /home/goat/nixos-dotfiles/modules/theming.mod.nix
          ];
          mod = builtins.foldl' (acc: file: acc // (import file)) {} files;
        in [
          inputs.home-manager.nixosModules.home-manager
          inputs.stylix.nixosModules.stylix
          inputs.grub2-themes.nixosModules.default
          {
            mod.nix-${host}
            mod.nix-global
            home-manager.sharedModules = [
              mod.home-${host}
              mod.home-global
              inputs.nixvim.homeManagerModules.nixvim
            ];
          }
        ];
      };
  in {
    # systems
    nixosConfigurations.desktop = systemConfig "desktop";
    nixosConfigurations.laptop = systemConfig "laptop";
  };
}
