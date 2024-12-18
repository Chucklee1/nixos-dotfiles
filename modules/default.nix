{...}: {
  imports = [
    ./general/games.nix #
    ./general/programs.nix
    ./general/stylix.nix
    ./general/system.nix # main config

    ./wm/essentials.nix
    ./wm/niri.nix #
    ./wm/hyprland.nix #
    ./wm/waybar.nix #
  ];
}
