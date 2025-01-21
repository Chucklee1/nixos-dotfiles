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
    # qol
    #
    treesitter.enable = true; # tree, sitting?
    telescope.enable = true;
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
        ltex = {
          enable = true;
          settings = {
            enabled = ["latex" "markdown" "text" "tex" "gitcommit"];
            completionEnabled = true;
            language = "en-US";
          };
        };
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
    colorizer.enable = true;
    nix.enable = true;

    # markdown
    render-markdown.enable = true;

    # neorg
    neorg = {
      enable = true;
      telescopeIntegration.enable = true;
      settings = {
        load = {
          "core.concealer" = {
            config = {
              icon_preset = "varied";
            };
          };
          "core.defaults" = {
            __empty = null;
          };
          "core.dirman" = {
            config = {
              workspaces = {
                eng101 = "~/notes/eng101";
                code = "~/notes/code";
              };
            };
          };
        };
      };
    };

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
