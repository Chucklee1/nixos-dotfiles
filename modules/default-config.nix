{inputs, ...}: {
  imports = [
    # toggle modules
    ./GPU/nvidia.nix
    ./wayland/default.nix
    ./wayland/niri/niri.nix
    ./wayland/hyprland/hyprland.nix
    ./gamse.nix
    # the rest
    ./system.nix
    ./infasoftware.nix
  ];
}
