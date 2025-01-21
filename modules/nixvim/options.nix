{
  enable = true;

  globalOpts = {
    number = true;
    relativenumber = true;
    signcolumn = "yes";
    cursorline = true;
    ruler = true;
    gdefault = true;
    scrolloff = 5;
    splitright = true;
    splitbelow = true;
    mouse = "a";
    ignorecase = true;
    smartcase = true;

    # tab defaults
    tabstop = 2;
    shiftwidth = 2;
    softtabstop = 0;
    expandtab = true;
    smarttab = true;

    # clipboard
    clipboard = {
      providers = {
        wl-copy.enable = true;
        xsel.enable = true;
      };
      register = "unnamedplus";
    };
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
      event = ["VimEnter"];
      callback = {__raw = "function() if vim.fn.argv(0) == '' then require('telescope.builtin').find_files() end end";};
    }
  ];
}
