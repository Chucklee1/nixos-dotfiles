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
    /*
     reminder for myself
    yank lines -> +y
    paste -> +p
    cut/delete lines -> del/+d
    */
    # lazy clipboard
    {
      mode = ["n" "v"];
      action = "+y";
      key = "<C-S-c>";
      options = {
        noremap = true;
        silent = true;
      };
    }
    {
      mode = ["n" "v"];
      action = "+d";
      key = "<C-S-x>";
      options = {
        noremap = true;
        silent = true;
      };
    }
    {
      mode = ["n" "v"];
      action = "+p";
      key = "<C-S-v>";
      options = {
        noremap = true;
        silent = true;
      };
    }
    # undo 
    {
    mode = "n";
    key = "<C-u>";
    action = "<cmd>u<CR>";
    }
    # format text
    {
      mode = "n";
      key = "<leader>f";
      action = "<cmd>!alejandra %<CR>";
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
      mode = ["n" "t"];
      action = "<cmd>ToggleTerm<CR>";
      key = "<leader>t";
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
  ];
}
