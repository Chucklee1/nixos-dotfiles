{
  plugins = {
    lsp = {
      enable = true;
      servers = {
        asm_lsp.enable = true; # GAS/GO assembly
        bashls.enable = true;
        clangd.enable = true;
        lua_ls.enable = true;
        marksman.enable = true;
        nixd.enable = true;
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

    # completion
    cmp = {
      enable = true;
      autoEnableSources = true;
    };
  };

  # TODO code snippits
  /*
    plugins.luasnip = {
    enable = true;
    settings = {
      enable_autosnippets = true;
      store_selection_keys = "<Tab>";
    };
    fromSnipmate = [
      {
        paths = "${self}/assets/snippets/nix.snippets";
        include = ["nix"];
      }
    ];
  };
  */
}
