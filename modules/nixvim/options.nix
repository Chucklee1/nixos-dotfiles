{
  enable = true;
  globalOpts = {
    # interface
    number = true;
    relativenumber = true;
    signcolumn = "yes";
    cursorline = true;
    splitright = true;
    splitbelow = true;

    # Tab defaults (might get overwritten by an LSP server)
    tabstop = 2;
    shiftwidth = 2;
    softtabstop = 0;
    smarttab = true;
    expandtab = true;
    autoindent = true;
    smartindent = true;

    ignorecase = true;
    smartcase = true;
    mouse = "a";
    scrolloff = 5;

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

  globals.mapleader = " ";

  autoCmd = [
    {
      event = ["VimEnter"];
      callback = {__raw = "function() if vim.fn.argv(0) == '' then require('telescope.builtin').find_files() end end";};
    }
    {
      event = ["BufEnter" "BufWinEnter"];
      pattern = ["*.md" "*.mdx"];
      command = "MarkdownPreviewToggle";
    }
  ];
}
