{
  description = "i dont kow what im doing";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    home-manager.url = "github:nix-community/home-manager";
    home-manager.inputs.nixpkgs.follows = "nixpkgs";
    stylix.url = "github:danth/stylix";
    niri.url = "github:sodiboo/niri-flake";
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
        {nvidia.enable = true;}
      ];
    };
    # macbook profile
    nixosConfigurations.macbook = nixpkgs.lib.nixosSystem {
      system = "x86_64-linux";
      specialArgs = {inherit inputs;};
      modules = [
        ./modules/hardware/macbook.nix
        ./modules/default-config.nix
        {nvidia.enable = false;}
      ];
    };
  };
}
