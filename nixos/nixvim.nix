{inputs, ...}: {
  home.global = [
    inputs.nixvim.homeManagerModules.nixvim
    ({
      lib,
      pkgs,
      ...
    }: {
      programs.nixvim = {
        # ----- OPTIONS -----
        enable = true;
        defaultEditor = true;
        viAlias = true;
        vimAlias = true;
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
        };
        performance.byteCompileLua.enable = true;
      };

      # ----- PLUGINS -----
      programs.nixvim = {
        # dependancies
        extraPlugins = with pkgs.vimPlugins; [
          plenary-nvim
        ];
        plugins.web-devicons.enable = true;
        extraConfigLuaPre =
          /*
          lua
          */
          ''
            if vim.g.have_nerd_font then
              require('nvim-web-devicons').setup {}
            end
          '';

        # ui related
        plugins = {
          bufferline.enable = true;
          lualine.enable = true;
          noice.enable = true; # fancy cmd window
          scrollview.enable = true;
          # file explorer meets text editor
          oil = {
            enable = true;
            settings = {
              delete_to_trash = true;
              view_options.show_hidden = false;
            };
          };
        };

        # qol plugins
        plugins = {
          intellitab.enable = true;
          # highlighting like terms
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

          telescope.enable = true;
          colorizer.enable = true;
          which-key.enable = true;
          wilder.enable = true;
        };

        # tree, sitting?
        plugins = {
          treesitter = {
            enable = true;
            settings = {
              indent.enable = true;
              highlight.enable = true;
            };
            nixvimInjections = true;
          };
          treesitter-context.enable = true;
        };

        # language specific
        plugins = {
          nix.enable = true;
          render-markdown.enable = true;
          fugitive.enable = true; # remote git acess
          lazygit.enable = true;
          gitsigns.enable = true;
        };
        extraConfigLua = ''
          require("telescope").load_extension("lazygit")
        '';

        # lsp
        plugins = {
          lsp = {
            enable = true;
            servers = {
              asm_lsp.enable = true; # GAS/GO assembly
              bashls.enable = true;
              clangd.enable = true;
              lua_ls.enable = true;
              marksman.enable = true;
              nixd.enable = true;
              yamlls.enable = true;
            };
          };
          lsp-format.enable = true;
          none-ls = {
            enable = true;
            enableLspFormat = true;
            sources.formatting = {
              alejandra.enable = true;
              prettier.enable = true;
              shfmt.enable = true;
            };
          };
        };
        extraPackages = with pkgs; [
          asm-lsp
          lua-language-server
          nixd
        ];
      };

      # ----- KEYMAPS -----
      programs.nixvim = {
        globals.mapleader = " ";
        globals.maplocalleader = " ";
        keymaps = let
          mkNormalKeyMaps = list: let
            Elem = int: lib.elemAt (lib.splitString " : " list) int;
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
            "bd! : <leader-S-c>"
          ];
      };
    })
  ];
}
