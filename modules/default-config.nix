{inputs, ...}: {
  imports = [
    # toggle modules
    ./GPU/nvidia.nix
    ./wayland/niri.nix
    ./gamse.nix
    # the rest
    ./system.nix
    ./infasoftware.nix
  ];
}
