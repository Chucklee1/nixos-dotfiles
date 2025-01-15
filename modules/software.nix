{
  pkgs,
  def,
  ...
}:
def.module "default" {
  # -----------------------------------------------------------
  # packages
  # -----------------------------------------------------------
  environment.systemPackages = with pkgs; [
    # tools/deps
    gcc
    zenity
    libnotify
    libsecret
    wine
    wineWowPackages.stagingFull
    samba
    winetricks
    # language QOL
    alejandra
    nixd
    asm-lsp
    # cli
    ripgrep
    pciutils
    btop
    ncdu
    # web/net
    wget
    git
    curl
    # media/files
    file-roller
    p7zip
    pavucontrol
    v4l-utils
    # apps
    krita
    webcord
    spotify
    zoom-us
  ];

  # programs
  programs = {
    dconf.enable = true;
    xfconf.enable = true;
    firefox.enable = true;
    thunar = {
      enable = true;
      plugins = with pkgs.xfce; [
        thunar-archive-plugin
        thunar-volman
      ];
    };
  };

  home-manager.sharedModules = [
    {
      programs = {
        # git
        git = {
          enable = true;
          userEmail = "cooperkang4@gmail.com";
          userName = "Chucklee1 - remote";
        };
        # terminal emulator
        kitty = {
          enable = true;
          settings = {
            scrollback_lines = 2000;
            wheel_scroll_min_lines = 1;
            window_padding_width = 0;
            confirm_os_window_close = 0;
            window_border_width = "0px";
            tab_bar_edge = "top";
            tab_bar_margin_width = "0.0";
            tab_bar_style = "fade";
            placement_strategy = "top-left";
            hide_window_decorations = true;
          };
        };
        # shell
        bash = {
          enable = true;
          shellAliases = {
            v = "nvim";
            vi = "nvim";
            vim = "nvim";
            sv = "sudo nvim";
            nixc = "cd $HOME/nixos-dotfiles & nvim";
            dwmc = "cd $HOME/dwm & nvim";
            cg = "sudo nix-collect-garbage";
            update-laptop = "sudo nixos-rebuild switch --impure --show-trace --flake $HOME/nixos-dotfiles#laptop";
            update-desktop = "sudo nixos-rebuild switch --impure --show-trace --flake $HOME/nixos-dotfiles#desktop";
          };
        };
        oh-my-posh = {
          enable = true;
          enableBashIntegration = true;
          useTheme = "pure";
        };
      };
      # its cleaner putting it in its own program.
      programs.nixvim = {
        enable = true;
        globals.mapleader = " ";
        opts = {
          # general ui
          number = true;
          signcolumn = "yes";
          cursorline = true;
          scrolloff = 10;
          showmode = false;
          foldlevel = 99;
          # Enable mouse
          mouse = "a";
          # Search
          ignorecase = true;
          smartcase = true;
          ruler = true;
          # splits
          splitright = true;
          splitbelow = true;
          # Tabs
          tabstop = 2;
          shiftwidth = 2;
          softtabstop = 0;
          expandtab = true;
          smarttab = true;
          breakindent = true;
          # eyes
          clipboard = {
            providers = {
              wl-copy.enable = true;
              xsel.enable = true;
            };
            register = "unnamedplus";
          };
          undofile = true;
        };
        keymaps = [
          # closing buffers
          {
            mode = "n";
            key = "<leader>c";
            action = "<cmd>bd<CR>";
          }
          {
            mode = "n";
            key = "<leader-S>c";
            action = "<cmd>bd!<CR>";
          }
          # force quit
          {
            mode = "n";
            key = "<leader-S>q";
            action = "<cmd>q!<cr>";
          }
          # move selected text
          {
            mode = ["n" "v"];
            key = "<A-Up>";
            action = ":m .+1<CR>==";
          }
          {
            mode = ["n" "v"];
            key = "<A-Down>";
            action = ":m .-2<CR>==";
          }
          # neo-tree
          {
            action = "<cmd>Neotree toggle<CR>";
            key = "<leader>e";
          }
          # git related
          {
            mode = "n";
            key = "<leader>gg";
            action = "<cmd>LazyGit<CR>";
          }
          # formatter for nix
          {
            mode = "n";
            key = "ff";
            action = "<cmd>!alejandra %<CR>";
          }
          # toggleterm
          {
            mode = ["n" "t"];
            key = "<leader>t";
            action = "<cmd>ToggleTerm<CR>";
          }
          # bufferline
          {
            mode = "n";
            key = "<Tab>";
            action = "<cmd>BufferLineCycleNext<cr>";
          }
          {
            mode = "n";
            key = "<S-Tab>";
            action = "<cmd>BufferLineCyclePrev<cr>";
          }
        ];

        plugins = {
          # icons
          web-devicons.enable = true;
          # ui
          bufferline.enable = true;
          lualine.enable = true;

          # File tree
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
          # qol
          treesitter = {
            enable = true;
            folding = true;
          };
          toggleterm = {
            enable = true;
            settings = {
              hide_numbers = true;
              autochdir = true;
              close_on_exit = true;
            };
          };
          # git
          lazygit.enable = true;
          gitsigns = {
            enable = true;
            settings.current_line_blame = true;
          };
          # lsp
          nix.enable = true;
          render-markdown = {
            enable = true;
            autoLoad = true;
            settings = {
              enabled = true;
              code = {
                above = " ";
                below = " ";
                border = "thin";
                sign = false;
                width = "block";
              };
            };
          };
          lsp = {
            enable = true;
            servers = {
              clangd.enable = true;
              marksman.enable = true;
              nil_ls.enable = true;
              bashls.enable = true;
            };
          };
        };
      };
    }
  ];
}
def.module "desktop" {
  environment.systemPackages = with pkgs; [
    protonup-qt
    protontricks
    prismlauncher
    osu-lazer-bin
  ];

  programs.steam = {
    enable = true;
    remotePlay.openFirewall = true;
    dedicatedServer.openFirewall = true;
    localNetworkGameTransfers.openFirewall = true;
  };
  environment.variables.STEAM_EXTRA_COMPAT_TOOLS_PATHS = "~/.steam/root/compatibilitytools..d";
}
def.module "desktop" {
  programs.virt-manager.enable = true;
  virtualisation = {
    spiceUSBRedirection.enable = true;
    libvirtd = {
      onBoot = "ignore";
      onShutdown = "shutdown";
      enable = true;
      qemu = {
        package = pkgs.qemu_kvm;
        runAsRoot = true;
        swtpm.enable = true;
        ovmf = {
          enable = true;
          packages = [
            (pkgs.OVMF.override {
              secureBoot = true;
              tpmSupport = true;
            })
            .fd
          ];
        };
      };
    };
  };

  home-manager.sharedModules = [
    {
      dconf.settings."org/virt-manager/virt-manager/connections" = {
        autoconnect = ["qemu:///system"];
        uris = ["qemu:///system"];
      };
    }
  ];
}
