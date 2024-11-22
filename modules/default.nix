{inputs, pkgs, ...}: {
  imports = [
    ./nixos/nvidia.nix
    ./nixos/gamse.nix
    ./nixos/system.nix
    ./nixos/infasoftware.nix
  ];

  # must be put in global scope for hm niri to work
  nixpkgs.overlays = [inputs.niri.overlays.niri];
  programs.niri.enable = true;
  programs.niri.package = pkgs.niri-unstable;
  
  # global scope for hm
  home-manager = {
    useUserPackages = true;
    useGlobalPkgs = true;
    extraSpecialArgs = {inherit inputs;};
    users.goat.imports = [./home/home.nix];
  };
}
