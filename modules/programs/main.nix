{self, ...}: {
  global.nix = [
    ({
      machine,
      ifSys,
      ...
    }: {
      environment = let
        root = "$HOME/nixos-dotfiles";
        buildFlags = "--show-trace --impure";
        buildType = ifSys.darwin "darwin" "nixos";
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
    })
  ];
  macbook = {
    nix = [
      ({config, ...}:
        with config.lib.stylix.colors; let
        in {
          services.jankyborders = {
            active_color = ''0xFF${base0D}'';
            inactive_color = ''0x00${base0D}'';
            style = "round";
            width = 2.0;
          };
        })
    ];
    home = [
      {
        home.file.".hammerspoon".source = "${self}/assets/hammerspoon";
        home.file.".config/sketchybar".source = "${self}/assets/sketchybar";
      }
    ];
  };

  global.home = [
    {
      home.file.".config/rmpc".source = "${self}/assets/rmpc";
      programs = {
        git.userEmail = "kermitthefrog@kakao.com";
        git.userName = "Chucklee1";
        lazygit.settings.notARepository = "skip";
        lazygit.settings.promptToReturnFromSubprocess = false;
        oh-my-posh.useTheme = "pure";
      };
    }
  ];
}
