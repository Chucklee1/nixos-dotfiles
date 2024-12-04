{...}: {
  imports = [
    ./software.nix
    ./theming.nix
    # toggle modules #
    ./programs/vscode.nix
  ];
}
