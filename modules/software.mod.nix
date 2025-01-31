{
  config,
  lib,
  pkgs,
  inputs,
  def,
  ...
}: let
  modules = [
    "nixvim"
    "wine"
    "steam"
    "wayland"
    "niri"
  ];

  mk = import ./libs.nix {inherit lib modules;};
in {
  options = mk.opts;
  config = lib.mkMerge [
    # -----------------------------------------------------------
    # global packages
    # -----------------------------------------------------------
    {
      environment.systemPackages = with pkgs; [
        # dependancies
        libnotify
        libsecret
        # development
        python3
        gnumake
        gdb
        gcc
        gimp
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
        webcord
        spotify
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
                confirm_os_window_close = 0;
                hide_window_decorations = true;
              };
            };
            # shell
            bash = {
              enable = true;
              shellAliases = {
                cg = "nix-collect-garbage";
                flake = "$HOME/nixos-dotfiles/flake.nix";
                update-flake = "sudo nixos-rebuild switch --impure --show-trace --flake $HOME/nixos-dotfiles#${def.host}";
              };
            };
            oh-my-posh = {
              enable = true;
              enableBashIntegration = true;
            };
          };
        }
      ];
    }

    # -----------------------------------------------------------
    # neovim
    # -----------------------------------------------------------
    (lib.mkIf config.nixvim.enable {
      home-manager.sharedModules = [
        inputs.nixvim.homeManagerModules.nixvim
        {programs.nixvim = import ./neovim.config.nix;}
      ];
    })

    # -----------------------------------------------------------
    # wine
    # -----------------------------------------------------------
    (lib.mkIf config.wine.enable {
      environment.systemPackages = with pkgs; [
        zenity
        samba
        wine
        wineWowPackages.stagingFull
        winetricks
        protonup-qt
        protontricks
      ];
    })

    # -----------------------------------------------------------
    # steam
    # -----------------------------------------------------------
    (lib.mkIf config.steam.enable {
      wine.enable = true;
      programs.steam = {
        enable = true;
        remotePlay.openFirewall = true;
        dedicatedServer.openFirewall = true;
        localNetworkGameTransfers.openFirewall = true;
      };
      environment.variables.STEAM_EXTRA_COMPAT_TOOLS_PATHS = "~/.steam/root/compatibilitytools..d";
    })

    # -----------------------------------------------------------
    # wayland
    # -----------------------------------------------------------
    (lib.mkIf config.niri.enable {
      wayland.enable = true;
      nixpkgs.overlays = [inputs.niri.overlays.niri];
      home-manager.sharedModules = [./niri.config.nix];

      programs.niri = {
        enable = true;
        package = pkgs.niri-unstable;
      };
    })
    # input imported this way to ensure it stays top-level
    (lib.mkIf config.wayland.enable {
      environment.systemPackages = with pkgs; [
        egl-wayland
        qt5.qtwayland
        qt6.qtwayland
        brightnessctl
        wev
        xwayland
        xwayland-run
        wl-color-picker
        wl-clipboard
      ];
      xdg.portal = {
        enable = true;
        extraPortals = [
          pkgs.xdg-desktop-portal-gtk
          pkgs.xdg-desktop-portal-gnome
        ];
        config.common.default = "*";
      };
      home-manager.sharedModules = [
        {
          programs = {
            fuzzel.enable = true;
            wpaperd.enable = true;
            swaylock = {
              enable = true;
              package = pkgs.swaylock-effects;
            };
            waybar = let
              colorWithHash = lib.mapAttrs (name: color: "#${color}") config.lib.stylix.colors;
              span = color: icon: "<span color='${color}' >${icon}</span>";
            in
              with colorWithHash; {
                enable = true;
                systemd.enable = true;
                settings = [
                  {
                    position = "top";
                    layer = "top";
                    height = 20;

                    modules-left = [
                      "niri/window"
                    ];
                    modules-center = [
                      "clock#date"
                      "custom/seperator"
                      "clock#time"
                    ];
                    modules-right = [
                      "idle_inhibitor"
                      "pulseaudio"
                      "network"
                      "backlight"
                      "battery"
                      "tray"
                    ];

                    "clock#date" = {
                      format = "{:%Y-%M-%d}";
                      tooltip = false;
                    };
                    "custom/seperator" = {
                      format = " | ";
                      tooltip = false;
                    };
                    "clock#time" = {
                      format = "{:%H:%M:%S}";
                      interval = 1;
                      tooltip = false;
                    };

                    idle_inhibitor = {
                      format = "{icon}";
                      format-icons = {
                        activated = span base0C "";
                        deactivated = "";
                        tooltip = false;
                      };
                    };
                    pulseaudio = {
                      format = "{volume}% {icon}";
                      format-bluetooth = "{volume}% {icon}";
                      format-icons = {"default" = ["" "" ""];};
                      format-muted = span base08 "M ";
                      format-source = "{volume}% ";
                      format-source-muted = span base08 "M ";
                      on-click = "pavucontrol";
                      tooltip = false;
                    };
                    network = {
                      format-disconnected = "disconnected ⚠";
                      format-ethernet = "{ipaddr}/{cidr}";
                      format-wifi = "{essid} ({signalStrength}%) ";
                      on-click = "nmtui";
                      tooltip-format = "{ifname} via {gwaddr}";
                    };
                    backlight = {
                      format = ''{percent}% ${span base0A "{icon}"}'';
                      format-icons = ["" "" "" "" "" "" "" "" ""];
                    };
                    battery = {
                      interval = 30;
                      states = {
                        warning = 40;
                        critical = 20;
                      };
                      format-icons = [" " " " " " " " " "];
                      format = ''{capacity}% ${span base0B "{icon}"}'';
                      format-warning = ''{capacity}% ${span base09 "{icon}"}'';
                      format-critical = ''{capacity}% ${span base08 "{icon}"}'';
                      format-charging = ''{capacity}% ${span base0B "󱐋{icon}"}'';
                      format-charging-warning = ''{capacity}% ${span base09 "󱐋{icon}"}'';
                      format-charging-critical = ''{capacity}% ${span base08 "󱐋{icon}"}'';
                      format-alt = "{icon} {time}";
                      tooltip = false;
                    };
                  }
                ];
                style = ''
                  * {
                    font-family: "JetBrainsMono nerd font";
                    font-size: 12px;
                  }
                  #window {
                    padding-left: 2px;
                  }

                  #idle_inhibitor,
                  #pulseaudio,
                  #network,
                  #backlight,
                  #battery {
                    padding: 0 10px 0 10px;
                  }
                '';
              };
          };
        }
      ];
    })
  ];
}
