{
  description = "i dont kow what im doing";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    home-manager.url = "github:nix-community/home-manager";
    home-manager.inputs.nixpkgs.follows = "nixpkgs";
    disko.url = "github:nix-community/disko/latest";
    disko.inputs.nixpkgs.follows = "nixpkgs";
    stylix.url = "github:danth/stylix";
    niri.url = "github:sodiboo/niri-flake";
    grub2-themes.url = "github:vinceliuice/grub2-themes";
  };

  outputs = { self, disko, nixpkgs, ... } @ inputs: let
    system = "x86_64-linux";
    pkgs = import nixpkgs {inherit system;};
    wallpaper = pkgs.fetchurl {
      url = https://raw.githubusercontent.com/Chucklee1/nixos-dotfiles/refs/heads/main/Pictures/mono-forest.PNG;
      sha256 = "0clzh6jwi2ph7mjabzkh7aq1q9jzlhmzr6nr9q97jlf5a39js80s";
    };
    specialArgs = {inherit inputs wallpaper;};
    shared-modules = [
      ./modules/default.nix
      inputs.home-manager.nixosModules.home-manager
      inputs.disko.nixosModules.disko
      inputs.stylix.nixosModules.stylix
      inputs.niri.nixosModules.niri
      inputs.grub2-themes.nixosModules.default
    ];
  in {
    # -----------------------------------------------------------
    # desktop profile
    # -----------------------------------------------------------
    nixosConfigurations.desktop = nixpkgs.legacyPackages.x86_64-linux.nixos {
      system = system;
      specialArgs = specialArgs;
      modules = shared-modules ++ [
        ./modules/hardware/desktop.nix
        {
          vscode.enable = false;
          niri.enable = true;
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
      modules = shared-modules ++ [
          ./modules/hardware/laptop.nix
          {
            vscode.enable = false;
            niri.enable = true;
            nvidia.enable = false;
            radeon.enable = true;
          }
        ];
    };
  };
}
# small notes:
# - default order parameters - { lib, config, pkgs, inputs, specialArgs, ... }
# # next to imports path = toggle module
# install commands
#sudo nix --experimental-features "nix-command flakes" run 'github:nix-community/disko/latest#disko-install' -- --write-efi-boot-entries --flake '/tmp/config/etc/nixos#mymachine' --disk main /dev/sda
#sudo nix --experimental-features "nix-command flakes" run 'github:nix-community/disko/latest#disko-install' -- --write-efi-boot-entries --flake 'github:Chucklee1/nixos-dotfiles#desktop' --disk main /dev/nvme0n1



