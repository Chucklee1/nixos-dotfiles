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
      the = hostname: host == hostname;
      it = {  
        laptop = nixpkgs.lib.optional (the "laptop");
        desktop = nixpkgs.lib.optional (the "desktop");
        both = nixpkgs.lib.optional (the "desktop" || the "laptop");
      };
      its = {
        laptop = nixpkgs.lib.optionalAttrs (the "laptop");
        desktop = nixpkgs.lib.optionalAttrs (the "desktop");
        both = nixpkgs.lib.optionalAttrs (the "desktop" || the "laptop");
      };
    };

    # system declaration
    systemConfig = host: (nixpkgs.lib.nixosSystem {
      specialArgs = {
        inherit inputs def; 
        is.the = is host;
      };
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
