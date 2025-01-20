{
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
      # move selected text
      {
        mode = "n";
        key = "<A-Up>";
        action = ":m .-2<CR>==";
      }
      {
        mode = "n";
        key = "<A-Down>";
        action = ":m .+1<CR>==";
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
      # closing buffers
      {
        mode = "n";
        key = "<leader>c";
        action = "<cmd>bd<CR>";
      }
      {
        mode = "n";
        key = "<leader-S>c";
        action = "<cmd>bd!<CR>";
      }
      # file explorer
      {
        action = "<cmd>Ex<CR>";
        key = "<leader>e";
      }
      # lazygit
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
      # make hokey
      {
        mode = "n";
        key = "<leader>m";
        action = "<cmd>!make<CR>";
      }
      # toggleterm
      {
        mode = ["n" "t"];
        key = "<leader>t";
        action = "<cmd>ToggleTerm<CR>";
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
      gitsigns.enable = true;
      gitsigns.settings.current_line_blame = true;
      # lsp
      nix.enable = true;
      render-markdown = {
        enable = true;
        autoLoad = true;
        settings = {
          enabled = true;
          code = {
            above = " ";
            below = " ";
            border = "thin";
            sign = false;
            width = "block";
          };
        };
      };
      lsp = {
        enable = true;
        servers = {
          clangd.enable = true;
          marksman.enable = true;
          nil_ls.enable = true;
          bashls.enable = true;
        };
      };
    };
  };
}
