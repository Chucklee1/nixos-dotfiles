{pkgs, ...}: {
  # ---- LSP ----
  plugins.lsp.enable = true;
  lsp.servers = {
    marksman.enable = true;
    nixd.enable = true;
    clangd.enable = true;
    jdtls.enable = true; # java
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
