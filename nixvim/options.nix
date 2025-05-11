{
  # ----- OPTIONS -----
  withPerl = false;
  withRuby = false;
  opts = {
    # visual opts
    number = true;
    relativenumber = true;
    signcolumn = "yes";
    cursorline = true;
    scrolloff = 5;
    splitright = true;
    splitbelow = true;
    termguicolors = true;
    # spacing behavior
    tabstop = 2;
    shiftwidth = 2;
    softtabstop = 0;
    smarttab = true;
    expandtab = true;
    breakindent = true;
    autoindent = true;
    smartindent = true;
    # case sensing
    ignorecase = true;
    smartcase = true;
    # idk
    mouse = "a";
    timeoutlen = 600;

    # history
    clipboard = {
      providers = {
        wl-copy.enable = true; # wayland
        xsel.enable = true; # X11
      };
      register = "unnamedplus";
    };
    backup = false;
    swapfile = false;
    undofile = true;
  };
  # command aliases
  userCommands = {
    Q.command = "q";
    Wq.command = "wq";
    WQ.command = "wq";
    W.command = "w";
    Ex.command = "Oil";
  };
  performance.byteCompileLua.enable = true;
}
