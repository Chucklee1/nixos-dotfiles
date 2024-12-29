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
      {
        mode = "n";
        key = "<leader>gg";
        action = "<cmd>LazyGit<CR>";
      }
      {
        mode = "n";
        key = "<leader>f";
        action = "<cmd>!alejandra %<CR>";
      }
      {
        mode = "n";
        key = "<leader>t";
        action = "<cmd>ToggleTerm<CR>";
      }
      {
        mode = "n";
        key = "<leader>c";
        action = "<cmd>bd<CR>";
      }
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

      {
        mode = "n";
        key = "<S-l>";
        action = "<cmd>BufferLineCycleNext<cr>";
      }

      {
        mode = "n";
        key = "<S-h>";
        action = "<cmd>BufferLineCyclePrev<cr>";
      }

      {
        mode = "n";
        key = "<leader>bd";
        action = "<cmd>bdelete<cr>";
      }
    ];
    plugins = {
      # icons
      web-devicons.enable = true;
      # ui
      bufferline.enable = true;
      lualine.enable = true;
      # qol
      treesitter.enable = true;
      toggleterm = {
        enable = true;
        settings = {
          hide_numbers = false;
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
    };
  };
}
