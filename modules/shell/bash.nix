{
  nix = [
    {
      programs.bash = {
        enable = true;
      };
        
      # Average "simple" posix complient shell prompt
      environment.variables.PS1 = "\033[31m┌─[\033[0m\033[1m\u\033[31m]\033[35m \w\n\033[31m└>\033[0m ";
    }
  ];
}
