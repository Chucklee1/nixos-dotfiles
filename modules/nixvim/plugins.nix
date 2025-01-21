{
  plugins = {
    #
    # interface sepetator
    #

    web-devicons.enable = true; # icon support
    bufferline.enable = true; # tabs
    lualine.enable = true; # status bar
    noice.enable = true; # fancy command pop-up
    oil.enable = true; # better file explorer

    #
    # language awareness seperator
    #

    lsp = {
      enable = true;
      servers = {
        bashls.enable = true; # bash
        nixd.enable = true; # nix
        clangd.enable = true; # C/C++
        asm_lsp.enable = true; # GAS/GO assembly
      };
    };

    # using just for formatting
    none-ls = {
      enable = true;
      sources.formatting = {
        prettier.enable = true; # a lot
        shfmt.enable = true; # shell
        alejandra.enable = true; # nix
      };
    };

    # debug support
    dap = {
      enable = true;
    };

    treesitter.enable = true; # tree, sitting?

    # language specific
    colorizer.enable = true;
    nix.enable = true;

    # git
    lazygit.enable = true;
    gitsigns.enable = true;
  };

  #
  # extra lua config sperator
  #
  extraConfigLuaPre = ''
    if vim.g.have_nerd_font then
      require('nvim-web-devicons').setup {}
    end
  '';
}
