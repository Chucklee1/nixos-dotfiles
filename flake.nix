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
    wallpapers = {
      TES5mountains.url = "https://w.wallhaven.cc/full/85/wallhaven-85wvmy.jpg";
      TES6mountains.hash = "0980sdgk3v351rsswyxgyw3rjjhym15fpa23xc8c8z5jfq49zvhk";
      waveout.url = "https://github.com/Chucklee1/nixos-dotfiles/blob/main/assets/wallpaper.png?raw=true";
      waveout.hash = "1za3bpr6m974j87f470pp1vmn3rxl0p1jic2mkvsy7qbkx3gccin";
    };
    def = {
      username = "goat";
      system = "x86_64-linux";
      wallpaper = waveout;
      layout = "us";
    };

    # system declaration
    systemConfig = host: (nixpkgs.lib.nixosSystem {
      system = def.system;
      specialArgs = {inherit inputs def;};
      modules = [
        ./modules/default.nix
        shared.modules
        ${host}.modules
        inputs.home-manager.nixosModules.home-manager
        {
          home-manager = [universal.home-modules];
          home-manager.sharedModules = [
            shared.home-modules
            ${host}.home-modules
          ];
        }
      ];
    });
  in {
    # systems
    nixosConfigurations.desktop = systemConfig "desktop";
    nixosConfigurations.laptop = systemConfig "laptop";
  };
}
