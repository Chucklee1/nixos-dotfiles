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
    def = {
      username = "goat";
      system = "x86_64-linux";
      layout = "us";
      wallpaper = ./assets/wallpaper.png;
    };

    # system declaration
    systemConfig = host: (nixpkgs.lib.nixosSystem {
      specialArgs = {inherit inputs def host;};
      modules = [
        ./modules/shared/default.mod.nix
        ./modules/shared/default.home.nix
        inputs.home-manager.nixosModules.home-manager
        inputs.stylix.nixosModules.stylix
        inputs.grub2-themes.nixosModules.default
        {home-manager.sharedModules = [inputs.nixvim.homeManagerModules.nixvim];}
      ];
    });
  in {
    # systems
    nixosConfigurations.desktop = systemConfig "desktop";
    nixosConfigurations.laptop = systemConfig "laptop";
  };
}
