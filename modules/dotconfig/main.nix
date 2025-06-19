{
  home.global = [
    ({
      lib,
      pkgs,
      machine,
      ...
    }: {
      programs = {
        git = {
          userEmail = "kermitthefrog@kakao.com";
          userName = "Chucklee1";
        };
        kitty = {
          settings = {
            confirm_os_window_close = 0;
            tab_bar_edge = "bottom";
            tab_bar_style = lib.mkForce "powerline";
            tab_powerline_style = "round";
          };
        };
        lazygit.settings = {
          notARepository = "skip";
          promptToReturnFromSubprocess = false;
        };
        yazi = {
          plugins =
            lib.genAttrs (pn: pkgs.yaziPlugins.${pn})
            ["mediainfo" "mount" "restore"];
        };
      };
      home = let
        root = /media/goat/BLUE_SATA/repos/nixos-dotfiles;
      in {
        shellAliases = {
          cg = "nix-collect-garbage";
          update-flake = "nix flake update --flake ${root}";
          rebuild-flake = "sudo nixos-rebuild switch -v --impure --show-trace --flake ${root}#${machine}";
        };
      };
    })
  ];
}
