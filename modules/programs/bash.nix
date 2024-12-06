{pkgs, ...}: {
  home-manager.sharedModules = [
    {
      programs.bash = {
        enable = true;
        shellAliases = {
          sv = "sudo nvim";
          cg = "sudo nix-collect-garbage";
          update = "sudo nixos-rebuild switch --impure --show-trace --flake";
          laptop = ".#laptop";
          desktop = ".#desktop";
        };
      };
    }
  ];
}
