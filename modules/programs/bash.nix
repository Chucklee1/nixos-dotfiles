{pkgs, ...}: {
  home-manager.sharedModules = [
    {
      programs.bash = {
        enable = true;
        shellAliases = {
          sv = "sudo nvim";
          cg = "sudo nix-collect-garbage";
          update-laptop = "sudo nixos-rebuild switch --impure --show-trace --flake ./nixos-dotfiles#laptop";
          update-desktop = "sudo nixos-rebuild switch --impure --show-trace --flake ./nixos-dotfiles#desktop";
          remote-update-laptop = "sudo nixos-rebuild switch --impure --show-trace --flake github:Chucklee1/nixos-dotfiles#laptop";
          remote-update-desktop = "sudo nixos-rebuild switch --impure --show-trace --flake github:Chucklee1/nixos-dotfiles#desktop";
        };
      };
    }
  ];
}
