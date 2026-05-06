{pkgs, ... }: { 
  # ---- LSP ----
  plugins.lsp.enable = true;
  lsp.servers = {
    # all profiles
    bashls.enable = true;
    lua_ls.enable = true;
    marksman.enable = true;
    nixd.enable = true;
    # full profile only
    asm_lsp.enable = true; # GAS/GO assembly
    clangd.enable = true;
    html.enable = true;
    jdtls.enable = true; # java
    qmlls.enable = true;
  };

  # ---- LANG QOL ----
  plugins = {
    # color preview
    colorizer = {
      enable = true;
      settings.user_default_options.names = false;
    };

    # inline code
    otter.enable = true;

    # document tools
    render-markdown.enable = true;
  };

  extraPlugins = with pkgs.vimPlugins; [plenary-nvim];
}
