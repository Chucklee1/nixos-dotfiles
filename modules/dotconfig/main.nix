let
  globalNix = {
    machine,
    user,
    ...
  }: {
    environment = let
      root =
        if machine == "macbook"
        then "/Users/${user}/nixos-dotfiles"
        else "/media/goat/BLUE_SATA/repos/nixos-dotfiles";
    in {
      variables = {
        BASH_SILENCE_DEPRECATION_WARNING = "1";
        TERMINAL = "kitty";
        EDITOR = "nvim";
      };
      shellAliases = {
        cg = "nix-collect-garbage";
        update-flake = "nix flake update --flake ${root}";
        rebuild-flake = "sudo nixos-rebuild switch --impure --show-trace --flake ${root}#${machine}";
      };
    };
  };
in {
  nix.global = [globalNix];
  nix.macbook = [globalNix];
  home.global = [
    ({
      lib,
      pkgs,
      ...
    }: {
      programs = {
        git = {
          userEmail = "kermitthefrog@kakao.com";
          userName = "Chucklee1";
        };
        lazygit.settings = {
          notARepository = "skip";
          promptToReturnFromSubprocess = false;
        };
        oh-my-posh.useTheme = "pure";
        yazi = {
          plugins =
            lib.genAttrs ["mediainfo" "mount" "restore"] (pn: pkgs.yaziPlugins.${pn});
        };
      };
    })
  ];
}
