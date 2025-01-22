{
  enable = true;
  globalOpts = {
    # lines
    number = true;
    relativenumber = true;
    signcolumn = "yes";
    cursorline = true;
    scrolloff = 5;

    # bannana __
    splitright = true;
    splitbelow = true;

    # tabs
    tabstop = 2;
    shiftwidth = 2;
    softtabstop = 0;
    smarttab = true;
    expandtab = true;

    # indents
    autoindent = true;
    smartindent = true;

    # cases
    ignorecase = true;
    smartcase = true;

    # mouse
    mouse = "a";

    # history
    clipboard = {
      providers = {
        wl-copy.enable = true; # Wayland
        xsel.enable = true; # For X11
      };
      register = "unnamedplus";
    };
    swapfile = false;
    undofile = true;
  };

  userCommands = {
    Q.command = "q";
    Q.bang = true;
    Wq.command = "q";
    Wq.bang = true;
    WQ.command = "q";
    WQ.bang = true;
    W.command = "q";
    W.bang = true;
  };

  autoCmd = [
    {
      event = ["BufEnter" "BufWinEnter"];
      pattern = ["*.md" "*.mdx"];
      command = "MarkdownPreviewToggle";
    }
  ];
}
