{...}: {
  imports = [
    ./general/games.nix
    ./general/system.nix # main config
    ./general/virt.nix
    ./configs/hyprland.nix
    ./configs/kitty.nix
    ./configs/niri.nix
    ./configs/shelli.nix
    ./configs/theme.nix
    ./configs/vscode.nix
    ./configs/waybar.nix
  ];
}
