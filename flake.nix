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

  outputs = {
    self,
    nixpkgs,
    ...
  } @ inputs: let
    system = "x86_64-linux";
    pkgs = import nixpkgs {inherit system;};
    defaults = {
      username = "goat";
      timeZone = "America/Vancouver";
      locale = "en_CA.UTF-8";
      wallpaper = ./assets/wallpaper.PNG;
      terminal = "kitty";
      file-manager = "thunar";
    };
    specialArgs = {inherit inputs system defaults;};
    shared-modules = with inputs;
      [
        home-manager.nixosModules.home-manager
        stylix.nixosModules.stylix
        niri.nixosModules.niri
        grub2-themes.nixosModules.default
      ]
      ++ [./modules/default.nix];
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
          ./modules/machines/desktop.nix
          {
            niri.enable = true;
            bspwm.enable = true;
            theme-minimal.enable = true;
            theme-fancy.enable = false;
            AMD.enable = true;
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
      modules =
        shared-modules
        ++ [
          ./modules/machines/laptop.nix
          {
            niri.enable = true;
            bspwm.enable = false;
            theme-minimal.enable = true;
            theme-fancy.enable = false;
            AMD.enable = true;
            nvidia.enable = false;
            radeon.enable = true;
          }
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
        ./assets/lazy-installer.sh "$@"'';
    in
      pkgs.symlinkJoin {
        name = name;
        paths = [script] ++ [pkgs.parted pkgs.git];
        buildInputs = [pkgs.makeWrapper];
        postBuild = "wrapProgram $out/bin/${name} --prefix PATH : $out/bin";
      };
  };
}
