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
  # math equations
  plugins.nabla.enable = true;
  extraConfigLuaPost = ''
    require"nabla".enable_virt({
      autogen = true, -- auto-regenerate ASCII art when exiting insert mode
      silent = true,     -- silents error messages
    })
  '';
}
