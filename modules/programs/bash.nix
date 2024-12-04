{pkgs, ...}: {
  home-manager.sharedModules = [
    {
      programs.bash = {
        enable = true;
        shellAliases = {
          sv = "sudo nvim";
          v = "nvim";
          start-pls = ". $HOME/nixos-dotfiles/dotconfig/set-home.sh";
          kittty = "kitty working-directory $HOME/nixos-dotfiles";
          ozonify = "--enable-features=UseOzonePlatform --ozone-platform=wayland";
          cg = "sudo nix-collect-garbage";
          update-desktop = "sudo nixos-rebuild switch --flake .#desktop --impure --show-trace";
          update-laptop = "sudo nixos-rebuild switch --flake .#laptop --impure --show-trace";
        };
      };
    }
  ];
}
