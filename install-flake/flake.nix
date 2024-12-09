{
  description = "small startup flake for simple one line install";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    disko.url = "github:nix-community/disko";
    disko.inputs.nixpkgs.follows = "nixpkgs";
    home-manager.url = "github:nix-community/home-manager";
    home-manager.inputs.nixpkgs.follows = "nixpkgs";
  };

  outputs = {
    self,
    nixpkgs,
    disko,
    ...
  } @ inputs: let
    specialArgs = {inherit disko inputs;};
  in {
    # -----------------------------------------------------------
    # minimal profile
    # -----------------------------------------------------------
    nixosConfigurations.sigma = nixpkgs.lib.nixosSystem {
      system = "x86_64-linux";
      specialArgs = specialArgs;
      modules = [
        disko.nixosModules.disko
        inputs.home-manager.nixosModules.home-manager
        ./config.nix
      ];
    };
  };
}
/*
command:
sudo nix /
--extra-experimental-features 'flakes nix-command' /
run github:nix-community/disko#disko-install -- --write-efi-boot-entries /
--disk main "/dev/disk-name" /
--flake "flake-name#profile-name" --show-trace
*/

