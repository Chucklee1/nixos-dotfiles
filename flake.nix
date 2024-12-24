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
    lib = import nixpkgs.lib;
    pkgs = import nixpkgs {inherit system;};
    username = "goat";
    wallpaper = ./assets/wallpaper.png;

    # system declaration
    systemConfig = host: (nixpkgs.lib.nixosSystem {
      system = {inherit system;};
      specialArgs = {inherit inputs system username wallpaper;};
      modules = [
        ./modules/shared
        ./modules/hosts/${host}
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
        name = {inherit name;};
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
