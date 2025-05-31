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

    blink-cmp = {
      enable = true;
      lazyLoad.settings.event = [
        "InsertEnter"
        "CmdlineEnter"
      ];
    };
    blink-cmp-dictionary.enable = true;
    blink-cmp-git.enable = true;
    blink-cmp-spell.enable = true;
    blink-copilot.enable = true;
    blink-emoji.enable = true;
    blink-ripgrep.enable = true;

    cmp = {
      enable = false;
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
        {name = "buffer";}
        {name = "nvim_lsp";}
        {name = "path";}
        {name = "latex-symbols";}
        {name = "treesitter";}
      ];
    };
  };
  extraPlugins = with pkgs.vimPlugins; [
    blink-cmp-conventional-commits
    blink-nerdfont-nvim
  ];

  extraPackages = with pkgs; [
    gh
    wordnet
  ];
}
