{
  nix = [
    {
      programs.bash = let
        set_attr = col: str: "\\033[${toString col}m${str}\\033[0m";
        red = str: set_attr 31 str;
        purple = str: set_attr 35 str;
        bf = str: set_attr 1 str;
      in {
        enable = true;
        promptInit = ''
          PS1='${red "┌─["}${bf "\\u"}${red "]"} ${purple "\\w"}\n${red "└>"} '
        '';
      };
    }
  ];
}
