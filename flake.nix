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
  } @ inputs: {
    # desktop profile
    nixosConfigurations.desktop = nixpkgs.lib.nixosSystem {
      system = "x86_64-linux";
      specialArgs = {inherit inputs;};
      modules = [
        ./modules/hardware/desktop.nix
        ./modules/default-config.nix
        {
          # toggle module options
          nvidia.enable = true;
          niri.enable = true;
          # boot resolution - MAKE SURE GRUB IS ENABLED
          boot.loader.grub.extraConfig = ''
            set gfxmode=1920x1080
            set gfxpayload=keep
          '';
        }
      ];
    };
    # macbook profile
    nixosConfigurations.macbook = nixpkgs.lib.nixosSystem {
      system = "x86_64-linux";
      specialArgs = {inherit inputs;};
      modules = [
        ./modules/hardware/macbook.nix
        ./modules/default-config.nix
        {
          nvidia.enable = false;
          niri.enable = true;
        }
      ];
    };
  };
}
# small notes:
# - default order parameters - { pkgs, inputs, lib, config, ... }

