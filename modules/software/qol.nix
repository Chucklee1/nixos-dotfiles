{
  home = [
    {
      programs.zoxide = {
        enable = true;
        options = ["--cmd cd"];
      };
      programs.nix-your-shell = {
        enable = true;
        nix-output-monitor.enable = true;
      };
      programs.eza.enable = true;
    }
  ];
}
