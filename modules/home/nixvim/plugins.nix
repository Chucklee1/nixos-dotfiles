{pkgs, ...}: {
  programs.nixvim = {    
    plugins = {
      # plugins sorted alphebetically

      # -- B --
      bufferline.enable = true;

      # -- G --
      gitsigns.enable = true;

      # -- I --
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

      intellitab.enable = true;

      # -- L --
      lazygit.enable = true; # git tui

      lualine.enable = true;

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

      # -- N --
      nix.enable = true;

      noice.enable = true; # fancy cmd window

      none-ls = {
        enable = true;
        enableLspFormat = true;
        sources.formatting = {
          alejandra.enable = true;
          prettier.enable = true;
          shfmt.enable = true;
        };
      };

      # -- O --
      oil = {
        # vim meets file-explorer
        enable = true;
        settings = {
          delete_to_trash = true;
          view_options.show_hidden = false;
        };
      };

      # -- R --
      render-markdown.enable = true;

      # -- S -- 
      scrollview.enable = true;

      # -- T --
      telescope.enable = true;

      treesitter = {
        # NOT an lsp
        enable = true;
        settings = {
          indent.enable = true;
          highlight.enable = true;
        };
        nixvimInjections = true;
      };

      treesitter-context.enable = true;

      # W
      web-devicons.enable = true;
      which-key.enable = true;
      wilder.enable = true;
    };

    extraPackages = with pkgs; [
      plenary-nvim
      asm-lsp
      lua-language-server
      nixd
    ];

    extraConfigLua = ''
      require("telescope").load_extension("lazygit")
    '';
    extraConfigLuaPre = ''
      if vim.g.have_nerd_font then
        require('nvim-web-devicons').setup {}
      end
    '';

  };
}