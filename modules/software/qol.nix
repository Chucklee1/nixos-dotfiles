{
  home = [
    {
      programs.zoxide = {
        enable = true;
        options = ["--cmd cd"];
      };
      programs.eza.enable = true;
    }
  ];
}
