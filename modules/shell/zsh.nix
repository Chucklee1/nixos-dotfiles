{
  nix = [
    ({pkgs, user, ...}: {
      users.users.${user}.shell = pkgs.zsh;
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
  ];
  home = [
    ({pkgs, ...}: {
      programs.zsh = {
        enable = true;
        syntaxHighlighting.enable = true;
        plugins = [
          {
            name = "zsh-vim-mode";
            src = pkgs.fetchFromGitHub {
              owner = "softmoth";
              repo = "zsh-vim-mode";
              rev = "1f9953b7d6f2f0a8d2cb8e8977baa48278a31eab";
              hash = "sha256-a+6EWMRY1c1HQpNtJf5InCzU7/RphZjimLdXIXbO6cQ=";
            };
          }
        ];
      };
    })
  ];
}
