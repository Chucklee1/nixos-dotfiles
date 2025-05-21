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

    # language qol

    # color preview
    colorizer = {
      enable = true;
      settings.user_default_options.names = false;
    };

    # nice keybinds for pairs
    sandwich.enable = true;

    # nix tools
    nix.enable = true;
    nix-develop.enable = true;

    # neat md previewer
    render-markdown.enable = true;

    # math equations
    nabla.enable = true;
  };
  # nabla keybind
  keymaps = [
    {
      key = "<leader>p";
      action.__raw = "require('nabla').popup";
    }
  ];
}
