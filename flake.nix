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
    system = "x86_64-linux";
    lib = nixpkgs.lib;
    pkgs = import nixpkgs {inherit system;};
    defaults = {
      username = "goat";
      wallpaper = ./assets/wallpaper.png;
    };

    # reads directory for .nix files
    import_modules = dir: let
      files = builtins.readDir dir;
    in
      builtins.mapAttrs (
        name: path: let
          absolutePath = builtins.toPath (dir + "/" + name); # Ensure the path is absolute
        in
          if lib.hasSuffix ".nix" name
          then builtins.import absolutePath
          else null
      )
      files;

    # system declaration
    systemConfig = host: (nixpkgs.lib.nixosSystem {
      system = system;
      specialArgs = {inherit inputs system defaults;};
      modules = [
        (import_modules "/home/${defaults.username}/nixos-dotfiles/modules/shared")
        (import_modules "/home/${defaults.username}/nixos-dotfiles/modules/hosts/${host}")
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
    });
  in {
    # systems
    nixosConfigurations.desktop = systemConfig "desktop";
    nixosConfigurations.laptop = systemConfig "laptop";

    # lazy install script
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
