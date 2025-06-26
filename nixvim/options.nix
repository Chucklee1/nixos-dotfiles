{
  # ----- OPTIONS -----
  nixpkgs.config.allowUnfree = true;
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

    # indentation
    smartindent = true;
    breakindent = true;

    # search
    hlsearch = true;
    incsearch = true;
    ignorecase = true;
    smartcase = true; # Don't ignore case with capitals
    grepprg = "rg --vimgrep";
    grepformat = "%f:%l:%c:%m";

    # spelling
    spelllang = ["en_us"]; # Spell check languages
    spell = false;

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

  # ---- AUTOCMDS ----
  # expandtab -> tabbing w/ spaces
  # noexpandtab -> tabbing w/ \t char
  autoCmd = [
    {
      desc = "enable spelling";
      event = ["FileType"];
      pattern = ["markdown" "latex" "html"];
      callback.__raw = "function() vim.opt.spell = true end,";
    }
    {
      desc = "4 tabspace, real tabs";
      event = ["FileType"];
      pattern = ["markdown" "latex" "html" "json" "toml" "yaml" "kdl"];
      command = "setlocal tabstop=4 shiftwidth=4 softtabstop=4 expandtab";
    }
    {
      desc = "2 tabspace, tabs are spaces";
      event = ["FileType"];
      pattern = ["nix" "lua" "asm" "c" "cpp"];
      command = "setlocal tabstop=2 shiftwidth=2 softtabstop=2 noexpandtab";
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
