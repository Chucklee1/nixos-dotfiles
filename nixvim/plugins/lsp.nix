{
  pkgs,
  lib,
  ...
}: {
  diagnostic.settings = {
    virtual_text = true;
    signs = true;
    underline = true;
    update_in_insert = false;
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
    lsp-format.enable = true;
    none-ls = {
      enable = true;
      enableLspFormat = true;
      sources.formatting = {
        alejandra.enable = true;
        prettier.enable = true;
        shfmt.enable = true;
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
      settings.mapping = {
        __raw = ''
          cmp.mapping.preset.insert({
            ['<C-d>'] = cmp.mapping.scroll_docs(-4),
            ['<C-f>'] = cmp.mapping.scroll_docs(4),
            ['<C-c>'] = cmp.mapping.abort(),
            ['<C-CR>'] = cmp.mapping.confirm({ select = true }),
          })
        '';
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
    vimtex = {
      enable = true;
      texlivePackage = pkgs.texlive.combined.scheme-full;
      settings.view_method = "zathura";
    };
  };
  globals.vimtex_compiler_latexmk.aux_dir = ".build";
}
