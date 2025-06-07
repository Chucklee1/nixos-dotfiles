{pkgs, ...}: {
  plugins = {
    # breadcrumbs
    lspsaga = {
      enable = true;
      lightbulb = {
        enable = false;
        virtualText = false;
      };
    };

    # completion
    cmp = {
      enable = true;
      settings = {
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
        window = {
          completion.__raw = ''cmp.config.window.bordered()'';
          documentation.__raw = ''cmp.config.window.bordered()'';
        };
      };
      autoEnableSources = true;
      settings.sources = [
        {name = "async-path";}
        {name = "buffer";}
        {name = "dictionary";}
        {name = "emoji";}
        {name = "luasnip";}
        {name = "latex-symbols";}
        {name = "nvim_lsp";}
        {name = "spell";}
        {name = "treesitter";}
        {name = "vimtex";}
      ];
    };

    # snippets
    nvim-snippets.enable = true;
    friendly-snippets.enable = true;
  };
  extraPackages = with pkgs; [wordnet];
}
