_: {
  home-manager.sharedModules = [
    {
      programs.nixvim = {
        enable = true;
        # options
        globalOpts = {
          number = true;
          signcolumn = "yes";
          tabstop = 2;
          shiftwidth = 2;
          # System clipboard
          clipboard = {
            providers = {
              wl-copy.enable = true;
              xclip.enable = true;
            };
            register = "unnamedplus";
          };
        };
        # keybinds
        globals.mapleader = " ";
        keymaps = [
          # neo-tree
          {
            action = "<cmd>Neotree toggle<CR>";
            key = "<leader>e";
          }

          # Lazygit
          {
            mode = "n";
            key = "<leader>gg";
            action = "<cmd>LazyGit<CR>";
            options = {
              desc = "LazyGit (root dir)";
            };
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
            key = "<leader>dd";
            action = "<cmd>bdelete<cr>";
            options = {
              desc = "Delete buffer";
            };
          }
        ];
        # plugins
        plugins = {
          web-devicons.enable = true; # icons
          treesitter.enable = true; # fancy styff
          telescope.enable = true; # fuzzy finder
          lazygit.enable = true;
          bufferline.enable = true; # tabs
          lualine.enable = true; # cool status bar
          noice.enable = true;  # fancy command window
          nix.enable = true; #nix expression support
          # built in terminal
          toggleterm = {
            enable = true;
            settings = {
              hide_numbers = false;
              autochdir = true;
              close_on_exit = true;
              direction = "vertical";
            };
          };
          # image support
          image = {
            enable = true;
            backend = "kitty";
            hijackFilePatterns = [
              "*.png"
              "*.jpg"
              "*.jpeg"
              "*.gif"
              "*.webp"
            ];
            maxHeightWindowPercentage = 25;
            integrations = {
              markdown = {
                enabled = true;
                downloadRemoteImages = true;
                filetypes = [
                  "markdown"
                  "vimwiki"
                  "mdx"
                ];
              };
            };
          };
          # file tree
          neo-tree = {
            enable = true;
            enableDiagnostics = true;
            enableGitStatus = true;
            enableModifiedMarkers = true;
            enableRefreshOnWrite = true;
            closeIfLastWindow = true;
            popupBorderStyle = "rounded"; # Type: null or one of “NC”, “double”, “none”, “rounded”, “shadow”, “single”, “solid” or raw lua code
            buffers = {
              bindToCwd = false;
              followCurrentFile = {
                enabled = true;
              };
            };
            window = {
              width = 40;
              height = 15;
              autoExpandWidth = false;
              mappings = {
                "<space>" = "none";
              };
            };
          };
          # language server
          none-ls = {
            enable = true;
            settings = {
              cmd = ["bash -c nvim"];
              debug = true;
            };
            sources = {
              code_actions = {
                statix.enable = true;
                gitsigns.enable = true;
              };
              diagnostics = {
                statix.enable = true;
                deadnix.enable = true;
                pylint.enable = true;
                checkstyle.enable = true;
              };
              formatting.alejandra.enable = true;
            };
          }; 
        };
      };
    }
  ];
}
