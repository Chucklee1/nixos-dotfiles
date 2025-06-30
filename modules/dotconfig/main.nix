{
  nix.global = [
    (
      {
        pkgs,
        machine,
        extlib,
        ...
      }: {
        environment = let
          root = "$HOME/nixos-dotfiles";
          buildFlags = "--show-trace --impure";
          buildType = extlib.ifDarwin pkgs.system "darwin" "nixos";
        in {
          variables = {
            BASH_SILENCE_DEPRECATION_WARNING = "1";
            TERMINAL = "kitty";
            EDITOR = "nvim";
          };
          shellAliases = {
            y = "yazi";
            ny = "cd ${root} && yazi";
            update-flake = "nix flake update --flake ${root}";
            rebuild-flake = "sudo ${buildType}-rebuild switch --flake ${root}#${machine} ${buildFlags}";
          };
        };
      }
    )
  ];
  home.global = [
    ({pkgs, ...}: {
      programs = {
        git.userEmail = "kermitthefrog@kakao.com";
        git.userName = "Chucklee1";
        lazygit.settings.notARepository = "skip";
        lazygit.settings.promptToReturnFromSubprocess = false;
        oh-my-posh.useTheme = "pure";
      };
    })
  ];
}
