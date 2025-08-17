{
  global.nix = [
    ({pkgs, ...}: {
      users.defaultUserShell = pkgs.zsh;
      environment.shells = with pkgs; [bash zsh];

      programs.zsh = {
        enable = true;
        promptInit = ''
          PROMPT="%F{red}"
          DIR=$'%F{magenta}%~\n'
          PS1="$PROMPT┌─[%f%n$PROMPT] $DIR$PROMPT└> %f"
        '';
      };
    })
    ({machine, ...}: {
      environment = {
        variables = {
          BASH_SILENCE_DEPRECATION_WARNING = "1"; # fix for macOS
          TERMINAL = "kitty";
          EDITOR = "nvim";
        };
        shellAliases = let
          rebuild_cmd =
            if machine == "macbook"
            then "darwin-rebuild"
            else "nixos-rebuild";
        in {
          rebuild-flake = "sudo ${rebuild_cmd} switch --flake $HOME/nixos-dotfiles#${machine} --show-trace --impure";
        };
      };
    })
  ];

  linux.nix = [{programs.zsh.syntaxHighlighting.enable = true;}];
  macbook.nix = [{programs.zsh.enableSyntaxHighlighting = true;}];
}
