{profile, ...}: {
  opts =
    if (profile != "full")
    then {}
    else {
      foldcolumn = "0";
      foldlevel = 99;
      foldlevelstart = 99;
      foldenable = true;
    };

  plugins.nvim-ufo = {
    enable = profile == "full";
    settings.provider_selector = ''
      function(bufnr, filetype, buftype)
        return {'treesitter', 'indent'}
      end
    '';
  };
}
