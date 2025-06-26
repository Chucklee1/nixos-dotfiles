{
  nix.global = [
    (
      {
        machine,
        system,
        extlib,
        ...
      }: {
        environment = let
          root = "$HOME/nixos-dotfiles";
          buildFlags = "--show-trace --impure";
          buildType =
            if extlib.isDarwin
            then "darwin"
            else "linux";
        in {
          variables = {
            BASH_SILENCE_DEPRECATION_WARNING = "1";
            TERMINAL = "kitty";
            EDITOR = "nvim";
          };
          shellAliases = {
            update-flake = "nix flake update --flake ${root}";
            rebuild-flake = "sudo ${buildType}-rebuild switch --flake ${root}#${machine} ${buildFlags}";
          };
        };
      }
    )
  ];
  home.global = [
    ({
      lib,
      pkgs,
      ...
    }: {
      programs = {
        git.userEmail = "kermitthefrog@kakao.com";
        git.userName = "Chucklee1";
        lazygit.settings.notARepository = "skip";
        lazygit.settings.promptToReturnFromSubprocess = false;
        oh-my-posh.useTheme = "pure";
        yazi.plugins = lib.genAttrs ["mediainfo" "mount" "restore"] (pn: pkgs.yaziPlugins.${pn});
      };
    })
  ];
}
