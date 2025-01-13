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
      wallpaper = ./assets/wallpaper.png;
    };

    # conditional host checker  
    is = host: rec {
      it = {  
        laptop = nixpkgs.lib.optional (host == "laptop");
        desktop = nixpkgs.lib.optional (host == "desktop");
        both = nixpkgs.lib.optional (host == "desktop" || host == "laptop");
      };
      its = {
        laptop = nixpkgs.lib.optionalAttrs (host == "laptop");
        desktop = nixpkgs.lib.optionalAttrs (host == "desktop");
        both = nixpkgs.lib.optionalAttrs (host == "desktop" || host == "laptop");
      };
    };

    # system declaration
    systemConfig = host: (nixpkgs.lib.nixosSystem {
      specialArgs = {
        inherit inputs def; 
        is = is host;
      };
      modules = [
        ./modules/default.nix
        ./modules/hosts/${host}/default.nix
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
