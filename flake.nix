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

  outputs = {
    nixpkgs,
    ...
  } @ inputs: let
    system = "x86_64-linux";
    pkgs = import nixpkgs {inherit system;};
    defaults = {
      username = "goat";
      wallpaper = ./assets/wallpaper.png;
      colors = {
        base00 = "#151515";
        base01 = "#202020";
        base02 = "#303030";
        base03 = "#505050";
        base04 = "#B0B0B0";
        base05 = "#D0D0D0";
        base06 = "#E0E0E0";
        base07 = "#F5F5F5";
        base08 = "#AC4142";
        base09 = "#D28445";
        base0A = "#F4BF75";
        base0B = "#90A959";
        base0C = "#75B5AA";
        base0D = "#6A9FB5";
        base0E = "#AA759F";
        base0F = "#8F5536";
      };
    };
    specialArgs = {inherit inputs system defaults;};
    shared-modules = [
      ./modules/default.nix
      inputs.home-manager.nixosModules.home-manager
      inputs.stylix.nixosModules.stylix
      inputs.niri.nixosModules.niri
      inputs.grub2-themes.nixosModules.default
      {
        home-manager.sharedModules = [
          inputs.nixvim.homeManagerModules.nixvim
        ];
      }
    ];
  in {
    # -----------------------------------------------------------
    # desktop profile
    # -----------------------------------------------------------
    nixosConfigurations.desktop = nixpkgs.lib.nixosSystem {
      system = system;
      specialArgs = specialArgs;
      modules =
        shared-modules
        ++ [
          ./modules/hosts/desktop/hardware.nix
          ./modules/hosts/desktop/config.nix
        ];
    };
    # -----------------------------------------------------------
    # laptop profile
    # -----------------------------------------------------------
    nixosConfigurations.laptop = nixpkgs.lib.nixosSystem {
      system = system;
      specialArgs = specialArgs;
      modules =
        shared-modules
        ++ [
          ./modules/hosts/laptop/hardware.nix
          ./modules/hosts/laptop/config.nix
        ];
    };
    # -----------------------------------------------------------
    # lazy-insaller script
    # -----------------------------------------------------------
    packages.x86_64-linux.lazy-installer = let
      name = "lazy-installer";
      script = pkgs.writeShellScriptBin name ''
        git clone https://github.com/Chucklee1/nixos-dotfiles 
        sleep 1
        cd nixos-dotfiles
        ./assets/lazy-installer.sh "$@"
      '';
    in
      pkgs.symlinkJoin {
        name = name;
        paths =
          [script]
          ++ [
            pkgs.parted
            pkgs.git
            pkgs.nixos-install-tools
          ];
        buildInputs = [pkgs.makeWrapper];
        postBuild = "wrapProgram $out/bin/${name} --prefix PATH : $out/bin";
      };
  };
}
