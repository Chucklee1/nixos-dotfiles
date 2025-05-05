{pkgs, ...}: {
  # ----- PLUGINS -----
  programs.nixvim = {
    # dependancies
    extraPlugins = with pkgs.vimPlugins; [
      plenary-nvim
    ];
    plugins.web-devicons.enable = true;
    extraConfigLuaPre =
      /*
      lua
      */
      ''
        if vim.g.have_nerd_font then
          require('nvim-web-devicons').setup {}
        end
      '';

    # ui related
    plugins = {
      #bufferline.enable = true;
      lualine.enable = true;
      noice.enable = true; # fancy cmd window
      scrollview.enable = true;
      # file explorer meets text editor
      oil = {
        enable = true;
        settings = {
          delete_to_trash = true;
          view_options.show_hidden = false;
        };
      };
    };

    # qol plugins
    plugins = {
      intellitab.enable = true;
      # highlighting like terms
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

      telescope.enable = true;
      colorizer.enable = true;
      which-key.enable = true;
      wilder.enable = true;
    };

    # tree, sitting?
    plugins = {
      treesitter = {
        enable = true;
        settings = {
          indent.enable = true;
          highlight.enable = true;
        };
        nixvimInjections = true;
      };
      treesitter-context.enable = true;
    };

    # language specific
    plugins = {
      nix.enable = true;
      render-markdown.enable = true;
      fugitive.enable = true; # remote git acess
      lazygit.enable = true;
      gitsigns.enable = true;
    };
    extraConfigLua = ''
      require("telescope").load_extension("lazygit")
    '';

    # lsp
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
    extraPackages = with pkgs; [
      asm-lsp
      lua-language-server
      nixd
    ];
  };
}
