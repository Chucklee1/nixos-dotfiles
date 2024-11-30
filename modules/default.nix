{
  inputs,
  pkgs,
  ...
}: {
  imports = [
    ./nixos/nvidia.nix
    ./nixos/gamse.nix
    ./nixos/system.nix
    ./nixos/infasoftware.nix
  ];
}
