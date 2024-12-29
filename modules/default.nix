host: {
  imports = [
    ./hosts/${host}/default.nix
    ./shared/user.nix
    ./shared/system.nix
    ./shared/software.nix
    ./shared/theming.nix
  ];
  home-manager.sharedModules = [
    ./home-shared/settings.nix
    ./home-shared/theming.nix
    ./home-shared/shelli.nix
    ./home-shared/niri.nix
    ./home-shared/nixvim.nix
    ./home-shared/vscode.nix
    ./home-shared/waybar.nix
  ];
}
