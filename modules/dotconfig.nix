{
  inputs,
  machine,
  ...
}: {
  nix.global = [
    inputs.home-manager.nixosModules.home-manager
    ({
      config,
      user,
      ...
    }: {
      home-manager.users.${user} = {
        home = {
          stateVersion = "24.05"; # DO NOT CHANGE
          username = "${config.users.users.${user}.name}";
          homeDirectory = "/home/${config.users.users.${user}.name}";
        };
        nixpkgs.config.allowUnfree = true;
        imports = config._module.args.homeMods;
      };
    })
  ];
  home.global = [
    ({lib, ...}: {
      home = {
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
        shellAliases =
          (lib.genAttrs ["v" "vi" "vm" "vim" "neovim"] (_: "nvim"))
          // {
            # nix - general
            cg = "nix-collect-garbage";
            update-flake = "nix flake update --flake $HOME/nixos-dotfiles";
            rebuild-flake = "sudo nixos-rebuild switch -v --impure --show-trace --flake $HOME/nixos-dotfiles#${machine}";
          };
        file = {
          # issue with nix shell
          ".config/nixpkgs/config.nix".text = "{ nixpkgs.config.allowUnfree = true; }";
        };
      };
    })
  ];
}
