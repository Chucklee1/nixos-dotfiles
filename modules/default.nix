{
  lib,
  config,
  pkgs,
  ...
}: {
  imports = [
    ./GPU/nvidia.nix
    ./GPU/radeon.nix
    ./system.nix
    ./general.nix
    ./niri.nix
    ./theming.nix
  ];
}