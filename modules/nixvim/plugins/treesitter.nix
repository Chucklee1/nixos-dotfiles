{
  home.global = [
    {
      plugins = {
        treesitter = {
          enable = true;
          settings = {
            indent.enable = true;
            highlight.enable = true;
          };
          nixvimInjections = true;
        };
        treesitter-context.enable = true;
      };
    }
  ];
}
