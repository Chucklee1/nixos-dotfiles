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
    # use nix-prefetch-url to get hash
    def = {
      username = "goat";
      system = "x86_64-linux";
      layout = "us";
      wallpaper = "https://raw.githubusercontent.com/Chucklee1/nixos-dotfiles/refs/heads/main/assets/wallpaper.png";
    };

    # system declaration
    systemConfig = host: (nixpkgs.lib.nixosSystem {
      system = def.system;
      specialArgs = {inherit inputs def;};
      modules = [
        ./modules/shared/default.nix
        ./modules/hosts/${host}/default.nix
        inputs.home-manager.nixosModules.home-manager
        inputs.stylix.nixosModules.stylix
        inputs.niri.nixosModules.niri
        inputs.grub2-themes.nixosModules.default
        {
          home-manager.sharedModules = [inputs.nixvim.homeManagerModules.nixvim];
        }
      ];
    });
  in {
    # systems
    nixosConfigurations.desktop = systemConfig "desktop";
    nixosConfigurations.laptop = systemConfig "laptop";
  };
}
