{...}: {
  imports = [
    # = togglable module
    #/ = lib.mkIf depenant module
    ./general/os-settings.nix
    ./general/infastructure.nix
    ./general/user.nix
    ./general/gpu.nix
    ./general/packages.nix
    ./general/theming.nix

    ./programs/thunar.nix
    ./programs/bash.nix
    ./programs/git.nix
    ./programs/kitty.nix
    ./programs/neovim.nix
    ./programs/vscode.nix #

    ./programs/niri/program.nix #
    ./programs/niri/deps.nix #/
    ./programs/niri/waybar.nix #/
    ./programs/niri/nvidia.nix #/
  ];
}
