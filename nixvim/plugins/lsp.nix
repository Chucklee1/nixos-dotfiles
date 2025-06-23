{pkgs, ...}: {
  # ---- LSP ----
  plugins.lsp.enable = true;
  lsp.servers = {
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

  # ---- FORMATTING ----
  plugins = {
    lsp-format.enable = true;
    none-ls = {
      enable = true;
      enableLspFormat = true;
      sources = {
        diagnostics.statix = {
          enable = true;
          settings.extra_args = ["--disable=duplicate_key"];
        };
        formatting = {
          alejandra.enable = true;
          prettier.enable = true;
          shfmt.enable = true;
        };
      };
    };

    # ---- LANG QOL ----

    # color preview
    colorizer = {
      enable = true;
      settings.user_default_options.names = false;
    };

    # document tools
    render-markdown.enable = true;
  };

  extraPlugins = with pkgs.vimPlugins; [
    plenary-nvim
  ];
  extraPackages = with pkgs; [
    stylua
    alejandra
  ];
}
