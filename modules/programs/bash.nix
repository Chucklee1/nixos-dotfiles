{pkgs, ...}: {
  home-manager.sharedModules = [
    {
      programs.bash = {
        enable = true;
        shellAliases = {
          sv = "sudo nvim";
          cg = "sudo nix-collect-garbage";
          update-laptop = "sudo nixos-rebuild switch --impure --show-trace --flake .#laptop";
          update-desktop = "sudo nixos-rebuild switch --impure --show-trace --flake .#desktop";
        };
      };
    }
  ];
}
