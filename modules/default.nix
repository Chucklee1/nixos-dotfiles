{inputs, ...}: {
  imports = [
    ./GPU/nvidia.nix
    ./wayland/niri.nix
    ./gamse.nix
    ./system.nix
    ./infasoftware.nix
  ];
}
