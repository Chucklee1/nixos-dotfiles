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
            wallpaper = ./assets/wallpaper.png;
          };
        };
        modules = [
          ./default.nix
          inputs.home-manager.nixosModules.home-manager
          inputs.stylix.nixosModules.stylix
          inputs.grub2-themes.nixosModules.default
          {
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
        ];
      };
  in {
    # systems
    nixosConfigurations.desktop = systemConfig "desktop";
    nixosConfigurations.laptop = systemConfig "laptop";
  };
}
