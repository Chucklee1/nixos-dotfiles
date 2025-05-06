{
  home.global = [
    {
      programs.nixvim.plugins.snacks = {
        enable = true;
        settings = {
          lazygit.enabled = true;
          indent.enabled = true;
          scroll.enabled = true;
          words.enabled = true;
        };
      };
    }
  ];
}
