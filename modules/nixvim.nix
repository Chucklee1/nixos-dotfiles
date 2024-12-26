_: {
  # -----------------------------------------------------------
  # global opts
  # -----------------------------------------------------------
  enable = true;
  globalOpts = {
    # general
    mouse = "a";
    clipboard = "null";
    undofile = true;

    # tabs
    shiftwidth = 2;
    softtabstop = 2;
    softtabwidth = 0;
    expandtab = true;
    smarttab = true;

    # UI config
    number = true; # show absolute number
    relativenumber = true; # add numbers to each line on the left side
    cursorline = true; # highlight cursor line underneath the cursor horizontally
    splitbelow = true; # open new vertical split bottom
    splitright = true; # open new horizontal splits right

    # Searching
    incsearch = true; # search as characters are entered
    hlsearch = false; # do not highlight matches
    ignorecase = true; # ignore case in searches by default
    smartcase = true; # but make it case sensitive if an uppercase is entered
  };

  # auto formatt nix files
  autoCmd = [
    {
      command = "silent! execute '!alajendra %'";
      event = [
        "BufWritePre"
      ];
      pattern = [
        "*.nix"
      ];
    }
  ];

  # -----------------------------------------------------------
  # plugins
  # -----------------------------------------------------------
  plugins = {
    bufferline.enable = true;
    web-devicons.enable = true;
    toggleterm.enable = true;
    lualine.enable = true;
    lazygit.enable = true;
  };

  # -----------------------------------------------------------
  # keymaps
  # -----------------------------------------------------------
  globals.mapleader = " ";
  keymaps = [
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
    {
      action = "<cmd>bd<CR>";
      key = "<leader>c";
      options = {
        noremap = true;
        silent = true;
      };
    }
  ];
}
