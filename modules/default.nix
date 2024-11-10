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
    ./software.nix
    ./niri.nix
    ./theming.nix
  ];
}
