{self, ...}: {
  global.nix = [
    ({machine, ...}: {
      #shell
      programs.zsh = {
        enable = true;
        syntaxHighlighting.enable = true;
        promptInit =
          #zsh
          ''
            if [ "$0" == "zsh" ]; then
              PROMPT="%F{red}"
              DIR=$'%F{magenta}%~\n'
              PS1="$PROMPT┌─[%f%n$PROMPT] $DIR$PROMPT└> %f"
            else
              PROMPT="\033[1;31m"
              DIR="\033[1;35m"
              ESC="\033[0m"
              PS1=$'%{PROMPT}┌─[$ESC\u$PROMPT] %{DIR}\w\n%{PROMPT}└> %{ESC}'
            fi
          '';
      };
      # global shell opts
      environment = {
        variables = {
          BASH_SILENCE_DEPRECATION_WARNING = "1";
          TERMINAL = "kitty";
          EDITOR = "nvim";
        };
        shellAliases = let
          rebuild_cmd =
            if machine == "macbook"
            then "darwin-rebuild"
            else "nixos-rebuild";
        in {
          y = "yazi";
          rebuild-flake = "sudo ${rebuild_cmd} switch --flake $HOME/nixos-dotfiles#${machine} --show-trace --impure";
        };
      };
    })
  ];
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
  macbook.nix = [
    ({config, ...}:
      with config.lib.stylix.colors; {
        services.jankyborders = {
          active_color = ''0xFF${base0D}'';
          inactive_color = ''0x00${base0D}'';
          style = "round";
          width = 3.0;
        };
      })
  ];
  macbook.home = [{home.file.".config/sketchybar".source = "${self}/assets/sketchybar";}];
}
