{pkgs, ...}: {
  # ---- LSP ----
  plugins.lsp.enable = true;
  lsp.servers = {
    asm_lsp.enable = true; # GAS/GO assembly
    bashls.enable = true;
    clangd.enable = true;
    html.enable = true;
    jdtls.enable = true;
    lemminx.enable = true; # xml
    lua_ls.enable = true;
    marksman.enable = true;
    nixd.enable = true;
  };

  # ---- LANG QOL ----
  plugins = {
    # color preview
    colorizer = {
      enable = true;
      settings.user_default_options.names = false;
    };

    # document tools
    render-markdown.enable = true;
  };

  extraPlugins = with pkgs.vimPlugins; [plenary-nvim];
  extraPackages = with pkgs; [
    alejandra
    haskell-language-server
    lemminx # xml lsp
    stylua
    vscode-langservers-extracted # soyjack lsps
  ];
}
