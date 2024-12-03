{...}: {
  imports = [
    ./packages.nix
    ./theming.nix
    ./programs/bash.nix
    ./programs/git.nix
    ./programs/neovim.nix
    ./programs/oh-my-posh.nix
    # toggle modules #
    ./programs/kitty.nix
    ./programs/vscode.nix
  ];
}
