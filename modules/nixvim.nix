{inputs, ...}: rec {
  #-------------------------
  #         OPTIONS
  #-------------------------
  opts.programs.nixvim = {
    enable = true;
    viAlias = true;
    vimAlias = true;
    withPerl = false;
    withRuby = false;
    opts = {
      # lines
      number = true;
      relativenumber = true;
      signcolumn = "yes";
      cursorline = true;
      scrolloff = 5;
      # windsplit
      splitright = true;
      splitbelow = true;
      # tabs
      tabstop = 2;
      shiftwidth = 2;
      softtabstop = 0;
      smarttab = true;
      expandtab = true;
      # indents
      breakindent = true;
      autoindent = true;
      smartindent = true;
      # cases
      ignorecase = true;
      smartcase = true;
      # mouse
      mouse = "a";
      # which key popup time
      timeoutlen = 600;
      # read
      termguicolors = true;

      # history
      clipboard = {
        providers = {
          wl-copy.enable = true; # Wayland
          xsel.enable = true; # For X11
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
    };
  };

  #-------------------------
  #         PLUGINS
  #-------------------------
  plugins.programs.nixvim.plugins = {
    # eye candy
    web-devicons.enable = true; # icon support
    noice.enable = true; # fancy command pop-up
    neoscroll.enable = true; # smoother scrolling
    illuminate = {
      enable = true;
      underCursor = false;
      filetypesDenylist = [
        "Outline"
        "TelescopePrompt"
        "alpha"
        "harpoon"
        "reason"
      ];
    };
    bufferline.enable = true; # tabs
    lualine.enable = true; # status bar
    oil.enable = true; # better file explorer

    # treesitting
    treesitter = {
      enable = true;
      settings = {
        indent.enable = true;
        highlight.enable = true;
      };
      nixvimInjections = true;
    };

    # qol plugins
    telescope.enable = true;
    colorizer.enable = true;
    which-key.enable = true;

    nix.enable = true; # nix expression
    render-markdown.enable = true; # markdown render

    lazygit.enable = true; # git menu
    gitsigns.enable = true; # git changes on left

    # lsp servers
    lsp = {
      enable = true;
      servers = {
        marksman.enable = true; # markdown
        yamlls.enable = true; # YAML
        bashls.enable = true; # bash
        nixd.enable = true; # nix
        clangd.enable = true; # C/C++
        asm_lsp.enable = true; # GAS/GO assembly
      };
    };

    # formatting
    lsp-format.enable = true;
    none-ls = {
      enable = true;
      enableLspFormat = true;
      sources.formatting = {
        prettier.enable = true; # a lot
        shfmt.enable = true; # shell
        alejandra.enable = true; # nix
      };
    };
  };

  #--------------------------
  #         KEYMAPS
  #--------------------------
  keymaps.programs.nixvim = {
    globals.mapleader = " ";
    globals.maplocalleader = " ";
    keymaps = [
      # file explorer
      {
        action = "<cmd>Oil<cr>";
        key = "<leader>e";
      }
      # Lazygit
      {
        mode = "n";
        key = "<leader>gg";
        action = "<cmd>LazyGit<CR>";
      }

      # Telescope bindings

      {
        action = "<cmd>Telescope live_grep<CR>";
        key = "<leader>fn";
      }
      {
        action = "<cmd>Telescope find_files<CR>";
        key = "<leader>f";
      }
      {
        action = "<cmd>Telescope git_commits<CR>";
        key = "<leader>fg";
      }
      {
        action = "<cmd>Telescope oldfiles<CR>";
        key = "<leader>fo";
      }

      # Bufferline bindings
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
        mode = "n";
        key = "<leader>c";
        action = "<cmd>bd<cr>";
      }
    ];
  };

  #-------------------------
  #         EXTRAS
  #-------------------------
  extras = {pkgs, ...}: {
    programs.nixvim = {
      extraPackages = with pkgs; [
        nixd
        asm-lsp
        nodePackages.prettier
        shfmt
        alejandra
      ];
      extraPlugins = [pkgs.vimPlugins.plenary-nvim];

      extraConfigLuaPre = ''
        if vim.g.have_nerd_font then
          require('nvim-web-devicons').setup {}
        end
      '';
      extraConfigLua = ''
        require("telescope").load_extension("lazygit")
      '';
    };
  };

  # merged plugin
  home = [
    inputs.nixvim.homeManagerModules.nixvim
    opts
    plugins
    keymaps
    extras
  ];
}
