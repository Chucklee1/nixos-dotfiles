{
  plugins = {
    lsp = {
      enable = true;
      servers = {
        asm_lsp.enable = true; # GAS/GO assembly
        bashls.enable = true;
        clangd.enable = true;
        html.enable = true;
        lua_ls.enable = true;
        marksman.enable = true;
        nixd.enable = true;
        texlab.enable = true;
        yamlls.enable = true;
      };
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

    # linting
    lint = {
      enable = true;
      lintersByFt = {
        c = ["cpplint"];
        cpp = ["cpplint"];
        nix = ["statix"];
        lua = ["selene"];
        python = ["flake8"];
        json = ["jsonlint"];
        bash = ["shellcheck"];
      };
    };

    # breadcrumbs
    lspkind.enable = true;
    lspsaga.enable = true;
    cmp = {
      enable = true;
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
    texpresso.enable = true; # cmd :TeXpresso
    render-markdown.enable = true;
  };
}
