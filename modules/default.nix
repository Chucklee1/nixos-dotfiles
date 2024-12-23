{...}: {
  imports = [
    ./general/system.nix # main config
    ./general/virt.nix

    ./configs/hyprland.nix
    ./configs/neovim.nix
    ./configs/niri.nix
    ./configs/shelli.nix
    ./configs/steam.nix
    ./configs/theme.nix
    ./configs/vscode.nix
    ./configs/waybar.nix
  ];
}
