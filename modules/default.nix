{...}: {
  imports = [
    ./desktop/config.nix #
    ./laptop/config.nix #

    ./niri.nix
    ./nixvim.nix
    ./shelli.nix #
    ./theme.nix
    ./vscode.nix #
    ./waybar.nix #
  ];
}
