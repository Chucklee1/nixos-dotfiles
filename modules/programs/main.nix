{self, ...}: {
  global.nix = [
    ({machine, ...}: {
      programs.bash.promptInit = ''
        PS1="\e[1;31m\]┌─[\[\e[0m\]\u\e[1;31m\]]\[\e[0m\] \[\e[1;35m\]\w\[\e[0m\]\n\[\e[1;31m\]└>\[\e[0m\] "
      '';
      environment = {
        variables = {
          BASH_SILENCE_DEPRECATION_WARNING = "1";
          TERMINAL = "kitty";
          EDITOR = "nvim";
        };
        shellAliases = {
          y = "yazi";
          rebuild-flake = "sudo nixos-rebuild switch --flake $HOME/nixos-dotfiles#${machine} --show-trace --impure";
        };
      };
    })
  ];
  macbook = {
    nix = [
      ({
        lib,
        config,
        machine,
        ...
      }:
        with config.lib.stylix.colors; let
        in {
          services.jankyborders = {
            active_color = ''0xFF${base0D}'';
            inactive_color = ''0x00${base0D}'';
            style = "round";
            width = 3.0;
          };
          shellAliases = {
            rebuild-flake = lib.mkOverride "sudo darwin-rebuild switch --flake $HOME/nixos-dotfiles#${machine} --show-trace --impure";
          };
        })
    ];
    home = [
      {
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
        kitty.shellIntegration.enableBashIntegration = false;
        kitty.settings = {
          cursor_shape = "beam";
          background_blur = 40;
          confirm_os_window_close = 0;
          tab_bar_edge = "bottom";
          tab_bar_style = "powerline";
          tab_powerline_style = "round";
        };
      };
    }
  ];
}
