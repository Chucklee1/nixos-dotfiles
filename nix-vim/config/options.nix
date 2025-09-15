{
  # ----- OPTIONS -----
  nixpkgs.config.allowUnfree = true;
  withNodeJs = false;
  withPerl = false;
  withPython3 = false;
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

    # indentation
    expandtab = true;
    smartindent = true;
    breakindent = true;

    # search
    hlsearch = true;
    incsearch = true;
    ignorecase = true;
    smartcase = true; # Don't ignore case with capitals
    grepprg = "rg --vimgrep";
    grepformat = "%f:%l:%c:%m";

    # highlight the uneeded
    list = true;
    listchars.__raw = "{ tab = '» ', trail = '·', nbsp = '␣' }";

    # spelling
    spelllang = ["en_us"]; # Spell check languages

    # history
    swapfile = false;
    backup = false;
    undofile = true;
  };
  clipboard = {
    providers = {
      wl-copy.enable = true; # wayland
      xsel.enable = true; # X11
    };
    register = "unnamedplus";
  };
  # command aliases
  userCommands = {
    Q.command = "q";
    Wq.command = "wq";
    WQ.command = "wq";
    W.command = "w";
  };
  performance.byteCompileLua.enable = true;

  # ---- AUTOCMDS ----
  # expandtab -> tabbing w/ spaces
  # noexpandtab -> tabbing w/ \t char
  autoCmd = [
    {
      desc = "enable spelling";
      event = ["FileType"];
      pattern = [
        "html"
        "latex"
        "markdown"
      ];
      callback.__raw = "function() vim.opt.spell = true end";
    }
    {
      desc = "4 tabspace, real tabs";
      event = ["FileType"];
      pattern = [
        "html"
        "json"
        "kdl"
        "latex"
        "markdown"
        "toml"
        "yaml"
        "xml"
        "sh"
      ];
      command = "setlocal tabstop=4 shiftwidth=4 softtabstop=4";
    }
    {
      desc = "2 tabspace, tabs are spaces";
      event = ["FileType"];
      pattern = [
        "asm"
        "c"
        "cpp"
        "lua"
        "nix"
      ];
      command = "setlocal tabstop=2 shiftwidth=2 softtabstop=2";
    }
  ];

  # ---- DIAGNOSTICS ----
  diagnostic.settings = {
    update_in_insert = true;
    severity_sort = true;
    virtual_text.current_line = true;
    float.border = "rounded";
    jump.severity.__raw = "vim.diagnostic.severity.WARN";
    signs.text = {
      "__rawKey__vim.diagnostic.severity.ERROR" = "";
      "__rawKey__vim.diagnostic.severity.WARN" = "";
      "__rawKey__vim.diagnostic.severity.HINT" = "󰌵";
      "__rawKey__vim.diagnostic.severity.INFO" = "";
    };
  };

  # ---- THEME ----
  colorschemes = {
    nightfox = {
      enable = true;
      flavor = "nordfox";
      settings.options.transparent = true;
    };
    catppuccin = {
      enable = false;
      settings = {
        flavor = "Frappe";
        transparent_background = true;
        default_integrations = true;
        integrations = {
          noice = true;
          lsp_trouble = true;
          which_key = true;
        };
      };
    };
  };
}
