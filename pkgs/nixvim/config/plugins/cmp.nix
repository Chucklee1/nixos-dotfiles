{
  opts.completeopt = [
    "menuone"
    "noselect"
    "noinsert"
  ];
  plugins = {
    # completion
    cmp.enable = true;
    cmp.autoEnableSources = true;
    cmp.settings = {
      mapping = {
        __raw = ''
          cmp.mapping.preset.insert({
            ['<C-d>'] = cmp.mapping.scroll_docs(-4),
            ['<C-f>'] = cmp.mapping.scroll_docs(4),
            ['<C-c>'] = cmp.mapping.abort(),
            ['<C-CR>'] = cmp.mapping.confirm({ select = true }),
          })
        '';
      };
      sources = [
        {name = "async-path";}
        {name = "emoji";}
        {name = "nvim_lsp";}
        {name = "treesitter";}
      ];
    };
  };
}
