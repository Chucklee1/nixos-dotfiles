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
    yamlls.enable = true;
  };

  # ---- FORMATTING ----
  plugins = {
    lsp-format.enable = true;
    none-ls = {
      enable = true;
      enableLspFormat = true;
      sources = {
        diagnostics.statix.enable = true;
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

    # nix tools
    nix.enable = true;
    nix-develop.enable = true;

    # document tools
    render-markdown.enable = true;

    # smart indenting
    sleuth.enable = true;
  };

  extraPlugins = with pkgs.vimPlugins; [
    plenary-nvim
  ];
  extraPackages = with pkgs; [
    # formatters
    stylua
    alejandra
  ];
}
