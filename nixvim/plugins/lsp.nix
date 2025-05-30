{
  pkgs,
  lib,
  ...
}: {
  diagnostic.settings = {
    virtual_text = true;
    underline = true;
  };
  plugins = {
    lsp = {
      enable = true;
      servers =
        lib.genAttrs
        [
          "asm_lsp" # GAS/GO assembly
          "bashls"
          "clangd"
          "html"
          "lua_ls"
          "marksman"
          "nixd"
          "texlab"
          "yamlls"
        ] (_: {enable = true;});
    };
    lsp-lines.enable = true;
    lsp-format.enable = true;
    none-ls = {
      enable = true;
      enableLspFormat = true;
      sources = {
        code_actions = {
          ts_node_action.enable = true;
        };
        formatting = {
          alejandra.enable = true;
          prettier.enable = true;
          shfmt.enable = true;
        };
      };
    };

    # breadcrumbs
    lspsaga = {
      enable = true;
      lightbulb = {
        enable = false;
        virtualText = false;
      };
    };
    cmp = {
      enable = true;
      settings = {
        completion.border = [
          "╭"
          "─"
          "╮"
          "│"
          "╯"
          "─"
          "╰"
          "│"
        ];
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

    # language qol

    # color preview
    colorizer = {
      enable = true;
      settings.user_default_options.names = false;
    };

    # nix tools
    nix.enable = true;
    nix-develop.enable = true;

    # document tools
    render-markdown.enable = true;
    ltex-extra.enable = true;
    vimtex = {
      enable = true;
      texlivePackage = pkgs.texlive.combined.scheme-full;
      zathuraPackage = pkgs.zathura;
      settings = {
        quickfix_ignore_filters = ["error"];
        quickfix_open_on_warning = 0;
        view_method = "zathura";
        compiler_latexmk = {
          aux_dir = ".build";
          options = [
            "-pdf"
            "-verbose"
            "-file-line-error"
            "-synctex=1"
            "-interaction=nonstopmode"
          ];
        };
      };
    };
  };
}
