{
  pkgs,
  lib,
  ...
}: {
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
      settings.view_method = "zathura";
    };
  };
  extraConfigLuaPre = ''
    vim.g.vimtex_compiler_latexmk = {
      aux_dir = ".build"
    }
    vim.g.vimtex_quickfix_ignore_filters = { 'warning' }
    vim.g.vimtex_quickfix_open_on_warning = 0
  '';
}
