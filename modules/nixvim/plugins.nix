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
    illuminate = {
      enable = true;
      underCursor = false;
      filetypesDenylist = [
        "Outline"
        "TelescopePrompt"
        "alpha"
        "harpoon"
        "reason"
      ];
    };

    #
    # qol
    # tree sitting?
    treesitter = {
      enable = true;
      settings = {
        indent.enable = true;
        highlight.enable = true;
      };
      nixvimInjections = true;
    };
    telescope.enable = true;
    colorizer.enable = true;
    which-key.enable = true;

    #
    # language awareness seperator
    #

    lsp = {
      enable = true;
      servers = {
        marksman.enable = true; # markdown
        yamlls.enable = true; # YAML
        bashls.enable = true; # bash
        nixd.enable = true; # nix
        clangd.enable = true; # C/C++
        asm_lsp.enable = true; # GAS/GO assembly
      };
    };

    # formatting
    lsp-format.enable = true;
    none-ls = {
      enable = true;
      enableLspFormat = true;
      sources.formatting = {
        prettier.enable = true; # a lot
        shfmt.enable = true; # shell
        alejandra.enable = true; # nix
      };
    };

    # language specific

    # nix
    nix.enable = true;

    # markdown
    markdown-preview.enable = true;
    render-markdown.enable = true;

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
  extraConfigLua = ''
    require("telescope").load_extension("lazygit")
  '';
}
