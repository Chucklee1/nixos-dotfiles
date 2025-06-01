{lib, ...}: {
  # ---- LSP ----
  plugins.lsp.enable = true;
  lsp.servers =
    {
      "*" = {
        settings = {
          capabilities = {
            textDocument = {
              semanticTokens = {
                multilineTokenSupport = true;
              };
            };
          };
          root_markers = [
            ".git"
          ];
        };
      };
    }
    // (lib.genAttrs
      [
        "asm_lsp" # GAS/GO assembly
        "bashls"
        "clangd"
        "html"
        "lua_ls"
        "ltex"
        "marksman"
        "nixd"
        "texlab"
        "yamlls"
      ] (_: {enable = true;}));

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
    render-markdown = {
      enable = true;
    };
  };
}
