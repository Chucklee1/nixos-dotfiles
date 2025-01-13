_: {
  # -----------------------------------------------------------
  # global opts
  # -----------------------------------------------------------
  programs.nixvim = {
    enable = true;
    globals.mapleader = " ";
    opts = {
      # general ui
      number = true;
      signcolumn = "yes";
      cursorline = true;
      scrolloff = 10;
      showmode = false;
      foldlevel = 99;
      # Enable mouse
      mouse = "a";
      # Search
      ignorecase = true;
      smartcase = true;
      ruler = true;
      # splits
      splitright = true;
      splitbelow = true;
      # Tabs
      tabstop = 2;
      shiftwidth = 2;
      softtabstop = 0;
      expandtab = true;
      smarttab = true;
      breakindent = true;
      # eyes
      clipboard = {
        providers = {
          wl-copy.enable = true;
          xsel.enable = true;
        };
        register = "unnamedplus";
      };
      undofile = true;
    };
    keymaps = [
      # builtins
            {
        mode = "n";
        key = "<leader>c";
        action = "<cmd>bd<CR>";
      }
      {
        mode = "n";
        key = "<leader>e";
        action = "<cmd>Ex<CR>";
      }
      # git related
      {
        mode = "n";
        key = "<leader>gg";
        action = "<cmd>LazyGit<CR>";
      }
      # formatter for nix
      {
        mode = "n";
        key = "ff";
        action = "<cmd>!alejandra %<CR>";
      }
      # toggleterm
      {
        mode = "n";
        key = "<leader>t";
        action = "<cmd>ToggleTerm<CR>";
      }
      # bufferline
      {
        mode = "n";
        key = "<Tab>";
        action = "<cmd>BufferLineCycleNext<cr>";
      }

      {
        mode = "n";
        key = "<S-Tab>";
        action = "<cmd>BufferLineCyclePrev<cr>";
      }
    ];
    plugins = {
      # icons
      web-devicons.enable = true;
      # ui
      bufferline.enable = true;
      lualine.enable = true;
      # qol
      treesitter = {
        enable = true;
        folding = true;
      };
      toggleterm = {
        enable = true;
        settings = {
          hide_numbers = true;
          autochdir = true;
          close_on_exit = true;
        };
      };
      # git
      lazygit.enable = true;
      gitsigns = {
        enable = true;
        settings.current_line_blame = true;
      };
      # lsp
      nix.enable = true;
      render-markdown = {
        enable = true;
        autoLoad = true;
      };
    };
  };
}
