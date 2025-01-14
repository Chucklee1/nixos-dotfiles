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
        specialArgs = let
          def = rec {
            username = "goat";
            hostname = "${host}-${username}";
            system = "x86_64-linux";
            wallpaper = ./assets/wallpaper.png;
          };
        in {
          inherit inputs host def;
        };
        modules = [./modules/default.nix];
      };
  in {
    # systems
    nixosConfigurations.desktop = systemConfig "desktop";
    nixosConfigurations.laptop = systemConfig "laptop";
  };
}
