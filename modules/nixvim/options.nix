_: {
  globals.mapleader = " ";
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
}
