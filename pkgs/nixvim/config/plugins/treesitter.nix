{
  opts = {
    foldcolumn = "0";
    foldlevel = 99;
    foldlevelstart = 99;
  };
  plugins = {
    treesitter = {
      enable = true;
      settings = {
        indent.enable = true;
        highlight.enable = true;
        incremental_selection.enable = true;
      };
    };
    nvim-ufo = {
      enable = true;
      settings.provider_selector = ''
        function(bufnr, filetype, buftype)
          return {'treesitter', 'indent'}
        end
      '';
    };
  };
}
