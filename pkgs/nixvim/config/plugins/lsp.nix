{
  pkgs,
  profile,
  ...
}: {
  # ---- LSP ----
  plugins.lsp.enable = true;
  lsp.servers = let
    full = (profile == "full");
  in (builtins.mapAttrs (_: v:
      v // {package = null;})
    {
      # all profiles
      bashls.enable = true;
      lua_ls.enable = true;
      marksman.enable = true;
      nixd.enable = true;
      # full profile only
      asm_lsp.enable = full; # GAS/GO assembly
      clangd.enable = full;
      html.enable = full;
      java.enable = full; # java
      jdtls.enable = full; # java
      lemminx.enable = full; # xml
      qmlls.enable = full;
      ts_ls.enable = full;
    });

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
}
