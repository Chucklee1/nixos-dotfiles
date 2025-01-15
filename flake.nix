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
    mkSystem = host: rec {
      # lazy naming
      system = "x86_64-linux";
      pkgs = import nixpkgs {inherit system;};
      lib = pkgs.lib;
      config = pkgs.config;
      def = {
        username = "goat";
        machine = {inherit host;};
        wallpaper = ./assets/wallpaper.png;
        module = typeArg: configSet:
          if (def.machine == typeArg || "default" == typeArg)
          then configSet
          else {};
        /*
        any module not defined as a one of the defined in
        nixosConfigurations or with default will be ignored,
        this is nice since you can easily disable modules by
        giving it another name, in this case I am using
        "disabled" or "off"
        */
      };

      # system functions
      mkSystem.metal = lib.nixosSystem rec {
        specialArgs = {inherit system inputs def;};

        modules = [
          inputs.stylix.nixosModules.stylix
          inputs.niri.nixosModules.niri
          inputs.grub2-themes.nixosModules.default
          ./software.nix
          ./system.nix
          ./theming.nix
          ./hardware.nix
          ./hosts/${host}.nix
          inputs.home-manager.nixosModules.home-manager
          {
            home-manager = {
              useUserPkgs = true;
              useGlobalPkgs = true;
              extraSpecialAgrs = {inherit specialArgs;};
              sharedModules = [inputs.nixvim.homeManagerModules.nixvim];
            };
          }
        ];
      };
    };
  in {
    # systems
    nixosConfigurations.desktop = mkSystem.metal "desktop";
    nixosConfigurations.laptop = mkSystem.metal "laptop";
  };
}
