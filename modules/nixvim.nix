{inputs, ...}: {
  home.global = [
    inputs.nixvim.homeManagerModules.nixvim
    {
      programs.nixvim = {
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
    }

    {
      programs.nixvim.plugins = {
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
    }
    ({lib, ...}:
      with lib; {
        programs.nixvim = {
          globals.mapleader = " ";
          globals.maplocalleader = " ";
          keymaps = let
            mkNormalKeyMaps = list: let
              Elem = int: elemAt (splitString " : " list) int;
            in {
              mode = "n";
              action = "<cmd>${Elem 0}<cr>";
              key = "${Elem 1}";
            };
          in
            # [ action key ]
            map mkNormalKeyMaps [
              # misc ui
              "Oil : <leader>e"
              "LazyGit : <leader>gg"

              # Telescope bindings
              "Telescope live_grep : <leader>fn"
              "Telescope find_files : <leader>f"
              "Telescope git_commits : <leader>fg"
              "Telescope oldfiles : <leader>fo"

              # Bufferline bindings
              "BufferLineCycleNext : <Tab>"
              "BufferLineCyclePrev : <S-Tab>"
              "BufferLineCycleNext : <S-l>"
              "BufferLineCyclePrev : <S-h>"
              "bd : <leader>c"
            ];
        };
      })

    ({pkgs, ...}: {
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
    })
  ];
}
