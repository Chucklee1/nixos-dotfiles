{
  # ----- OPTIONS -----
  withPerl = false;
  withRuby = false;
  opts = {
    # numbers/general
    number = true;
    relativenumber = true;
    signcolumn = "yes";
    showmode = false; # uneeded
    wrap = true;

    # mouse mode
    mouse = "a";

    # time
    timeoutlen = 800;
    updatetime = 50; # faster completion (4000ms default)

    # cursor
    cursorline = false;
    termguicolors = true; # 24 bit
    splitright = true;
    splitbelow = true;

    # ui spacing
    cmdheight = 2;
    scrolloff = 5;
    pumheight = 0;

    # tabs
    tabstop = 2;
    softtabstop = 2;
    showtabline = 2;
    expandtab = true;

    # indentation
    smartindent = true;
    shiftwidth = 2;
    breakindent = true;

    # search
    hlsearch = true;
    incsearch = true;
    ignorecase = true;
    smartcase = true; # Don't ignore case with capitals
    grepprg = "rg --vimgrep";
    grepformat = "%f:%l:%c:%m";

    # history
    clipboard = {
      providers = {
        wl-copy.enable = true; # wayland
        xsel.enable = true; # X11
      };
      register = "unnamedplus";
    };
    swapfile = false;
    backup = false;
    undofile = true;

    # completeopt for cmp
    completeopt = [
      "menuone"
      "noselect"
      "noinsert"
    ];

    # folding
    foldcolumn = "0";
    foldlevel = 99;
    foldlevelstart = 99;
    foldenable = true;
    foldmethod = "expr";
    foldexpr = "v:lua.vim.treesitter.foldexpr()";
  };
  # command aliases
  userCommands = {
    Q.command = "q";
    Wq.command = "wq";
    WQ.command = "wq";
    W.command = "w";
    Ex.command = "Oil";
    #texopen.command = "TeXpresso";
  };

  # ---- THEME ----

  colorschemes.nightfox = {
    enable = true;
    flavor = "nordfox";
    settings.options.transparent = true;
  };
}
