_: {
  # -----------------------------------------------------------
  # global opts
  # -----------------------------------------------------------
  enable = true;
  globalOpts = {
    # ui
    number = true;
    splitright = true;
    splitbelow = true;

    # Enable mouse
    mouse = "a";

    # Search
    ignorecase = true;
    smartcase = true;

    # shows tails
    list = true;
    # NOTE: .__raw here means that this field is raw lua code
    listchars.__raw = "{ tab = '» ', trail = '·', nbsp = '␣' }";

    # tabs
    tabstop = 2;
    shiftwidth = 2;
    softtabstop = 0;
    expandtab = true;
    smarttab = true;

    # System clipboard support, needs xclip/wl-clipboard
    clipboard = {
      providers = {
        wl-copy.enable = true; # Wayland
        xsel.enable = true; # For X11
      };
      register = "unnamedplus";
    };
  };

  # -----------------------------------------------------------
  # plugins
  # -----------------------------------------------------------
  plugins = {
    web-devicons.enable = true; # icon support
    bufferline.enable = true; # tabs
    toggleterm.enable = true; # intigrated terminal
    lualine.enable = true; # cool status bar
    lazygit.enable = true;
    gitsigns = {
      enable = true;
      settings.current_line_blame = true;
    };
    nix.enable = true; # nix expression support
  };

  # -----------------------------------------------------------
  # keymaps
  # -----------------------------------------------------------
  globals.mapleader = " ";
  keymaps = [
    # format write
    {
      mode = "n";
      key = "<leader>w";
      action = "<cmd>w<CR><cmd>!alejandra %<CR><cmd>w<CR>";
    }
    # lazygit
    {

      mode = "n";
      key = "<leader>gg";
      action = "<cmd>LazyGit<CR>";
      options = {
        desc = "LazyGit (root dir)";
      };
    }
    # toggleterm
    {
      mode = "n";
      action = "<cmd>ToggleTerm<CR>";
      key = "<leader>t";
      options = {
        noremap = true;
        silent = true;
      };
    }
    {
      mode = "t";
      action = "<cmd>ToggleTerm<CR>";
      key = "<leader>x";
      options = {
        noremap = true;
        silent = true;
      };
    }
    # bufferline
    {
      mode = "n";
      key = "<Tab>";
      action = "<cmd>BufferLineCycleNext<cr>";
      options = {
        desc = "Cycle to next buffer";
      };
    }

    {
      mode = "n";
      key = "<S-Tab>";
      action = "<cmd>BufferLineCyclePrev<cr>";
      options = {
        desc = "Cycle to previous buffer";
      };
    }

    {
      mode = "n";
      key = "<S-l>";
      action = "<cmd>BufferLineCycleNext<cr>";
      options = {
        desc = "Cycle to next buffer";
      };
    }

    {
      mode = "n";
      key = "<S-h>";
      action = "<cmd>BufferLineCyclePrev<cr>";
      options = {
        desc = "Cycle to previous buffer";
      };
    }
  ];
}
