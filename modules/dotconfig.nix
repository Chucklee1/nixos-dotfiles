{
  inputs,
  machine,
  user,
  ...
}: {
  nix.global = [
    inputs.home-manager.nixosModules.home-manager
    ({config, ...}: {
      home-manager.users.${user} = {
        home = {
          stateVersion = "24.05"; # DO NOT CHANGE
          username = user;
          homeDirectory = "/home/${user}";
        };
        nixpkgs.config.allowUnfree = true;
        imports = config._module.args.homeMods;
      };
    })
  ];
  home.global = [
    ({lib, ...}: {
      programs = {
        git = {
          enable = true;
          userEmail = "kermitthefrog@kakao.com";
          userName = "Chucklee1";
        };
        kitty = {
          enable = true;
          settings = {
            confirm_os_window_close = 0;
            tab_bar_edge = "bottom";
            tab_bar_style = lib.mkForce "powerline";
            tab_powerline_style = "round";
          };
        };
        bash.enable = true;
        oh-my-posh = {
          enable = true;
          useTheme = "pure";
        };
      };
      home = {
        shellAliases =
          (lib.genAttrs ["v" "vi" "vm" "vim" "neovim"] (_: "nvim"))
          // {
            cg = "nix-collect-garbage";
            update-flake = "nix flake update --flake $HOME/nixos-dotfiles";
            rebuild-flake = "sudo nixos-rebuild switch -v --impure --show-trace --flake $HOME/nixos-dotfiles#${machine}";
          };
        # issue with nix shell
        file.".config/nixpkgs/config.nix".text = "{ nixpkgs.config.allowUnfree = true; }";
      };
    })
  ];
}
