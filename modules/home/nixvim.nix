{
  # -----------------------------------------------------------
  # global opts
  # -----------------------------------------------------------
  programs.nixvim = {
    enable = true;
    globals.mapleader = " ";
    opts = {
      # general
      number = true;
      signcolumn = "yes";
      mouse = "a"; # mouse support
      showmode = false;
      cursorline = true;

      # clipboard & history
      undofile = true;
      clipboard = {
        providers = {
          wl-copy.enable = true; #
          xsel.enable = true; #
        };
        register = "unnamedplus";
      };

      # search
      ignorecase = true; # ignore case in search pattern
      smartcase = true; # override ignorecase if uppercase is used

      # split behavior
      splitright = true;
      splitbelow = true;

      # whitespaces
      list = true;
      listchars.__raw = "{ tab = '» ', trail = '·', nbsp = '␣' }";

      # Ttabs
      tabstop = 2;
      shiftwidth = 2;
      softtabstop = 0;
      expandtab = true;
      smarttab = true;

      # Text wrapping and scrolling
      breakindent = true;
      scrolloff = 10;
    };

    keymaps = [
      # Neo-tree bindings
      {
        action = "<cmd>Neotree toggle<CR>";
        key = "<leader>e";
      }
      # Undotree
      {
        mode = "n";
        key = "<leader>ut";
        action = "<cmd>UndotreeToggle<CR>";
      }
      # lazygit
      {
        mode = "n";
        key = "<leader>gg";
        action = "<cmd>LazyGit<CR>";
      }
      # bufferline
      {
        mode = "n";
        key = "<Tab>";
        action = "<cmd>BufferLineCycleNext<cr>";
        options.desc = "Cycle next buffer";
      }

      {
        mode = "n";
        key = "<S-Tab>";
        action = "<cmd>BufferLineCyclePrev<cr>";
        options.desc = "Cycle prev buffer";
      }

      {
        mode = "n";
        key = "<S-l>";
        action = "<cmd>BufferLineCycleNext<cr>";
        options.desc = "Cycle next buffer";
      }

      {
        mode = "n";
        key = "<S-h>";
        action = "<cmd>BufferLineCyclePrev<cr>";
        options.desc = "Cycle prev buffer";
      }
    ];
    plugins = {
      # general ui #
      nvim-web-devicons = true; # required for icons
      bufferline.enable = true;
      lualine.enable = true;
      # qol #
      persistence.enable = true;
      telescope.enable = true;
      toggleterm.enable = true;
      # git #
      lazygit.enable = true;
      gitsigns = {
        enable = true;
        settings.current_line_blame = true;
      };
      # file tree #
      neo-tree = {
        enable = true;
        enableDiagnostics = true;
        enableGitStatus = true;
        enableRefreshOnWrite = true;
      };
      undotree.enable = true;

      # lsp #
      nvim-treesitter = {
        enable = true;
        ensure_installed = ["c" "cpp" "rust" "asm"];
        highlight.enable = true;
        fold.enable = true; # code folding
      };
      lsp = {
        enable = true;
        servers = {
          clangd.enable = true; # C/C++
          rust_analyzer.enable = true; # Rust
          pyright.enable = true; # Python
          nil_ls.enable = true; # Nix
          bashls.enable = true; # Bash
          yamlls.enable = true; # YAML
        };
      };
    };
  };
}
