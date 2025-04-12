{
  programs = {
    btop.enable = true;

    bash = {
      enable = true;
      shellAliases = {
        cg = "nix-collect-garbage";
        update-flake = "nix flake update $HOME/nixos-dotfiles";
        rebuild-desktop = "sudo nixos-rebuild switch --impure --show-trace --flake $HOME/nixos-dotfiles#desktop";
        rebuild-laptop = "sudo nixos-rebuild switch --impure --show-trace --flake $HOME/nixos-dotfiles#laptop";
        rebuild-macbook = "darwin-rebuild switch --impure --show-trace --flake $HOME/nixos-dotfiles#macbookpro";
      };
    };
    oh-my-posh = {
      enable = true;
      enableBashIntegration = true;
      useTheme = "https://github.com/JanDeDobbeleer/oh-my-posh/blob/main/themes/pure.omp.json";
    };
  };
}
