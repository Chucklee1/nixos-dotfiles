{...}: {
  imports = [
    ./general/system.nix # main config
    ./general/virt.nix

    ./configs/nixvim/plugins.nix
    ./configs/nixvim/options.nix
    ./configs/nixvim/keymappings.nix

    ./configs/hyprland.nix
    ./configs/niri.nix
    ./configs/shelli.nix
    ./configs/steam.nix
    ./configs/theme.nix
    ./configs/vscode.nix
    ./configs/waybar.nix
  ];
}
