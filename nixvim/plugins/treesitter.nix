{
  plugins = {
    treesitter = {
      enable = true;
      settings = {
        indent.enable = true;
        highlight.enable = true;
        folding.enable = true;
      };
      nixvimInjections = true;
    };
  };
}
