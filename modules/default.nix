{...}: {
  # -----------------------------------------------------------
  # systems
  # -----------------------------------------------------------
  config = {
    desktop = [
      inputs.home-manager.nixosModules.home-manager
      ({
        config,
        lib,
        pkgs,
        def,
        ...
      }: {
        # -----------------------------------------------------------
        # system
        # -----------------------------------------------------------
        boot = {
          initrd.availableKernelModules = ["nvme" "xhci_pci" "ahci" "usb_storage" "usbhid" "sd_mod"];
          kernelModules = ["kvm-amd"];
          supportedFilesystems = ["ntfs"];
        };

        system.stateVersion = "24.05"; # DO NOT CHANGE
        networking = {
          interfaces.enp7s0.useDHCP = lib.mkDefault true;
          interfaces.wlp6s0.useDHCP = lib.mkDefault true;
        };

        hardware.cpu.amd.updateMicrocode = lib.mkDefault config.hardware.enableRedistributableFirmware;
        services.xserver.videoDrivers = ["nvidia"];
        hardware.nvidia = {
          powerManagement.enable = false;
          powerManagement.finegrained = false;
          open = false;
          package = config.boot.kernelPackages.nvidiaPackages.stable;
        };

        # -----------------------------------------------------------
        # packages
        # -----------------------------------------------------------
        environment.systemPackages = with pkgs; [
          # tools/deps
          nvidia-vaapi-driver
          zenity
          wineWowPackages.stagingFull
          samba
          winetricks
          protonup-qt
          protontricks
          # hyprland
          hyprland-protocols
          hyprshot
          hypridle
          hyprlock
          hyprsunset
          # games
          osu-lazer-bin
          prismlauncher
        ];

        # -----------------------------------------------------------
        # programs
        # -----------------------------------------------------------
        programs = {
          hyprland.enable = true;
          virt-manager.enable = true;
          dconf.enable = true;
          gamemode.enable = true;
          steam = {
            enable = true;
            remotePlay.openFirewall = true;
            dedicatedServer.openFirewall = true;
            localNetworkGameTransfers.openFirewall = true;
          };
        };

        # -----------------------------------------------------------
        # misc
        # -----------------------------------------------------------
        environment.variables = {STEAM_EXTRA_COMPAT_TOOLS_PATHS = "~/.steam/root/compatibilitytools.d";};
        virtualisation = {
          spiceUSBRedirection.enable = true;
          libvirtd = {
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

        # -----------------------------------------------------------
        # home manager
        # -----------------------------------------------------------
        home-manager.sharedModules = [
          {
            dconf.settings = {
              "org/virt-manager/virt-manager/connections" = {
                autoconnect = ["qemu:///system"];
                uris = ["qemu:///system"];
              };
            };
            wayland.windowManager.hyprland = {
              enable = true;
              plugins = [pkgs.hyprlandPlugins.hyprscroller];
              settings = {
                # general
                monitor = ", 1920x1080@165, auto, auto";
                xwayland.force_zero_scaling = true;
                input = {
                  kb_layout = "${def.layout}";
                  follow_mouse = 0;
                  sensitivity = 0;
                };
                gestures = {
                  workspace_swipe = false;
                  workspace_swipe_forever = false;
                };
                misc = {
                  disable_autoreload = true;
                  force_default_wallpaper = 0;
                  animate_mouse_windowdragging = false;
                };
                # window rules windowrule = <rule>,<info>
                # windowrulev2 = [];
                # startup
                exec-once = [
                  "waybar"
                  "nm-applet"
                  "hyprsunset -t 5200"
                  "swaybg -i $HOME/nixos-dotfiles/assets/wallpaper.png -m fill"
                ];
                # theming & layoutr
                general = {
                  layout = "scroller";
                  gaps_in = 2;
                  gaps_out = 4;
                  border_size = 2;
                  allow_tearing = true;
                  resize_on_border = true;
                };
                decoration = {
                  rounding = 4;
                  animations = {
                    # bezier = NAME, X0, Y0, X1, Y1
                    bezier = [
                      "ease-out-quad, 0.76, 0, 0.24, 1"
                      "ease-out-cubic, 0.33, 1, 0.68, 1"
                      "ease-out-expo, 0.16, 1, 0.3, 1"
                      "spring, 0.25, 0.75, 0.50, 1.0"
                    ];
                    # animation: NAME, ONOFF, SPEED, CURVE [,STYLE]
                    animation = [
                      "workspaces, 1, 1.5, spring"
                      "windows, 1, 1.5, spring"
                      "windowsIn, 1, 4, ease-out-expo" # open
                      "windowsOut, 1, 4, ease-out-quad" # close`
                      "windowsMove, 1, 2.9, spring"
                      "fade, 0"
                    ];
                  };
                };
                # bindings: MODS, key, dispatcher, params
                "$mod" = "SUPER";
                bind = [
                  # programs
                  "$mod, return, exec, kitty"
                  "$mod shift, return, exec, kitty -e tmux"
                  "$mod, space, exec, fuzzel"
                  "$mod, e, exec, thunar"
                  "$mod shift, p, exec, wlogout"
                  "$mod shift, l, exec, swaylock"
                  # hyprland
                  "$mod, v, togglefloating"
                  "$mod shift, m, fullscreen"
                  ", ctrl+alt+del, exit"
                  # scroller - overview windows
                  "$mod shift, space, scroller:toggleoverview"
                  # scroller - align windows
                  "$mod, h, scroller:alignwindow, l"
                  "$mod, l, scroller:alignwindow, r"
                  # scroller - set columns
                  "$mod shift, j, scroller:admitwindow"
                  "$mod shift, k, scroller:expelwindow"
                  # scroller - semi fullscreen
                  "$mod, m, scroller:fitsize, active"
                  # scroller - set window size
                  "$mod, equal, scroller:cyclewidth, next"
                  "$mod, minus, scroller:cyclewidth, prev"
                  "$mod SHIFT, equal, scroller:cycleheight, next"
                  "$mod SHIFT, minus, scroller:cycleheight, prev"
                  # windows
                  "$mod, Q, killactive"
                  "$mod, left, movefocus, l"
                  "$mod, right, movefocus, r"
                  "$mod, up, movefocus, u"
                  "$mod, down, movefocus, d"
                  "$mod shift, left, movewindow, l"
                  "$mod shift, right, movewindow, r"
                  "$mod shift, up, movewindow, u"
                  "$mod shift, down, movewindow, d"
                  # workspaces
                  "$mod ctrl, up, workspace, -1"
                  "$mod ctrl, down, workspace, +1"
                  "$mod shift ctrl, up, movetoworkspace, -1"
                  "$mod shift ctrl, down, movetoworkspace, +1"
                ];
                bindm = [
                  # mouse related
                  "$mod, mouse:272, movewindow"
                  "$mod, mouse:273, resizewindow"
                  "$mod ALT, mouse:272, resizewindow"
                ];
                bindl = [
                  # media keys
                  ", XF86AudioPlay, exec, playerctl play-pause"
                  ", XF86AudioPrev, exec, playerctl previous"
                  ", XF86AudioNext, exec, playerctl next"
                  ", XF86AudioMute, exec, wpctl set-mute @DEFAULT_AUDIO_SINK@ toggle"
                  ", XF86AudioMicMute, exec, wpctl set-mute @DEFAULT_AUDIO_SOURCE@ toggle"
                ];
                bindle = [
                  # why hyprland, must you have diffrent names for these
                  ", XF86AudioRaiseVolume, exec, wpctl set-volume -l '1.0' @DEFAULT_AUDIO_SINK@ 5%+"
                  ", XF86AudioLowerVolume, exec, wpctl set-volume -l '1.0' @DEFAULT_AUDIO_SINK@ 5%-"
                  ", XF86MonBrightnessUp, exec, brightnessctl s 5%+"
                  ", XF86MonBrightnessDown, exec, brightnessctl s 5%-"
                ];
              };
            };
          }
        ];
      })
    ];
    laptop = [
      inputs.home-manager.nixosModules.home-manager
      ({
        config,
        lib,
        ...
      }: {
        imports = [./hardware.nix];
        # boot
        boot = {
          initrd.availableKernelModules = ["nvme" "xhci_pci" "usb_storage" "sd_mod"];
          kernelModules = ["kvm-amd"];
        };

        # system options
        system.stateVersion = "24.05"; # DO NOT CHANGE
        networking.interfaces.wlp2s0.useDHCP = lib.mkDefault true;

        # hardware
        hardware.cpu.amd.updateMicrocode = lib.mkDefault config.hardware.enableRedistributableFirmware;
        services.xserver.videoDrivers = ["radeon"];
      })
    ];
    shared = [
      inputs.home-manager.nixosModules.home-manager
      inputs.niri.nixosModules.niri
      inputs.stylix.homeManagerModules.stylix
      inputs.grub2-theme.homeManagerModules.default
      ({
        config,
        lib,
        pkgs,
        inputs,
        def,
        ...
      }: {
        # -----------------------------------------------------------
        # boot
        # -----------------------------------------------------------
        boot.loader = {
          efi.canTouchEfiVariables = true;
          grub = {
            enable = true;
            efiSupport = true;
            device = "nodev";
          };
          grub2-theme = {
            enable = true;
            theme = "stylish";
            footer = true;
          };
        };

        # -----------------------------------------------------------
        # system options
        # -----------------------------------------------------------

        networking = {
          hostName = "${def.username}";
          networkmanager.enable = true;
          useDHCP = lib.mkDefault true;
        };

        i18n.defaultLocale = "en_CA.UTF-8";
        time.timeZone = "America/Vancouver";
        console = {
          earlySetup = true;
          keyMap = def.layout;
        };

        # -----------------------------------------------------------
        # nix options
        # -----------------------------------------------------------
        nixpkgs = {
          hostPlatform = lib.mkDefault "${def.system}";
          config.allowUnfree = true;
          overlays = [inputs.niri.overlays.niri];
        };
        nix.settings = {
          auto-optimise-store = true;
          experimental-features = ["nix-command" "flakes"];
        };

        # -----------------------------------------------------------
        # system user declaration
        # -----------------------------------------------------------
        users.users.${def.username} = {
          isNormalUser = true;
          extraGroups = ["wheel" "networkmanager" "libvirtd"];
        };

        # -----------------------------------------------------------
        # system packages
        # -----------------------------------------------------------
        environment.systemPackages = with pkgs; [
          # tools/deps
          gcc
          vulkan-tools
          ffmpeg
          v4l-utils
          libnotify
          libsecret
          # language QOL
          alejandra
          nixd
          asm-lsp
          # cli
          killall
          ripgrep
          pciutils
          btop
          ncdu
          # web/net
          wget
          git
          curl
          # wayland
          egl-wayland
          qt5.qtwayland
          qt6.qtwayland
          # window manager utils
          wev
          brightnessctl
          xclip
          wl-clipboard
          cliphist
          swaybg
          wlsunset
          networkmanagerapplet
          lxqt.lxqt-policykit
          # media
          mpv
          imv
          pavucontrol
          # apps/games
          firefox
          openmw
          # misc
          nerd-fonts.symbols-only
          file-roller
          p7zip
        ];

        # -----------------------------------------------------------
        # system programs
        # -----------------------------------------------------------
        programs = {
          niri.enable = true;
          niri.package = pkgs.niri-unstable;
          xfconf.enable = true; # for thunar config
          thunar = {
            enable = true;
            plugins = with pkgs.xfce; [
              thunar-archive-plugin
              thunar-volman
            ];
          };
        };

        # -----------------------------------------------------------
        # system infastrcuture
        # -----------------------------------------------------------

        # security and portals
        security = {
          polkit.enable = true;
          rtkit.enable = true; # for sound
        };
        xdg.portal = {
          enable = true;
          extraPortals = [pkgs.xdg-desktop-portal-gtk];
          config.common.default = ["gtk"];
        };

        # hardware
        hardware = {
          graphics.enable = true; # renamed opengl to graphics as of 24.11
          graphics.enable32Bit = true;
          bluetooth.enable = true;
          bluetooth.powerOnBoot = true;
        };

        # services
        services = {
          pipewire = {
            enable = true;
            alsa.enable = true;
            alsa.support32Bit = true;
            pulse.enable = true;
          };
          blueman.enable = true;
          fstrim.enable = true;
          displayManager.ly.enable = true;
          libinput.enable = true;
          printing.enable = true;
          openssh.enable = true;
          # for thunar
          tumbler.enable = true;
          gvfs.enable = true;
        };

        # -----------------------------------------------------------
        # stylix
        # -----------------------------------------------------------
        stylix = {
          enable = true;
          autoEnable = true;
          homeManagerIntegration.autoImport = true;
          image = pkgs.fetchurl {
            url = def.wallpaper.url;
            hash = def.wallpaper.hash or lib.fakeHash;
          };
          cursor.package = pkgs.bibata-cursors;
          cursor.name = "Bibata-Modern-Classic";
          cursor.size = 24;
          base16Scheme = "${pkgs.base16-schemes}/share/themes/classic-dark.yaml";

          fonts = {
            monospace = {
              package = pkgs.nerd-fonts.jetbrains-mono;
              name = "JetBrainsMono Nerd Font Mono";
            };
            sansSerif = {
              package = pkgs.noto-fonts-cjk-sans;
              name = "Noto Sans CJK";
            };
            serif = {
              package = pkgs.noto-fonts-cjk-serif;
              name = "Noto Serif CJK";
            };
            sizes = {
              applications = 12;
              terminal = 12;
              desktop = 11;
              popups = 12;
            };
          };
          targets.grub.enable = false;
        };

        # -----------------------------------------------------------
        # home manager
        # -----------------------------------------------------------
        home-manager.sharedModules = [
          inputs.nixvim.homeManagerModules.nivim
          {
            # -----------------------------------------------------------
            # general
            # -----------------------------------------------------------
            home.sessionVariables = {
              # wayland
              XDG_SESSION_TYPE = "wayland";
              CLUTTER_BACKEND = "wayland";
              QT_WAYLAND_DISABLE_WINDOWDECORATION = 1;
              # xwayland compat.
              DISPLAY = ":0";
              SDL_VIDEODRIVER = "x11";
              QT_QPA_PLATFORM = "wayland;xcb";
              GDK_BACKEND = "wayland,x11,*";
            };
            # most wm services
            services = {
              dunst.enable = true;
              gnome-keyring.enable = true;
            };

            # -----------------------------------------------------------
            # theming
            # -----------------------------------------------------------
            stylix.targets.waybar.enable = false;
            gtk = {
              enable = true;
              iconTheme.name = "Papirus-Dark";
              iconTheme.package = pkgs.papirus-icon-theme;
              gtk3.extraConfig.gtk-application-prefer-dark-theme = 1;
              gtk4.extraConfig.gtk-application-prefer-dark-theme = 1;
            };
            qt = {
              enable = true;
              style.name = "adwaita-dark";
              platformTheme.name = "gtk3";
            };

            # -----------------------------------------------------------
            # programs
            # -----------------------------------------------------------
            programs = {
              # wm
              fuzzel.enable = true;
              wlogout.enable = true;
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
                  window_padding_width = 4;
                  confirm_os_window_close = 0;
                  window_border_width = "0px";
                  tab_bar_edge = "top";
                  tab_bar_margin_width = "0.0";
                  tab_bar_style = "fade";
                  placement_strategy = "top-left";
                  hide_window_decorations = true;
                };
              };
              # -----------------------------------------------------------
              # shell
              # -----------------------------------------------------------
              bash = {
                enable = true;
                shellAliases = {
                  v = "nvim ./nixos-dotfiles";
                  vi = "nvim ./nixos-dotfiles";
                  vim = "nvim ./nixos-dotfiles";
                  sv = "sudo nvim";
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

              # -----------------------------------------------------------
              # vscode
              # -----------------------------------------------------------
              vscode = {
                enable = true;
                extensions = with pkgs.vscode-extensions; [
                  jnoortheen.nix-ide
                  kamadorueda.alejandra
                ];
                userSettings = {
                  "update.mode" = "none";
                  "update.enableWindowsBackgroundUpdates" = false;
                  "git.enableSmartCommit" = true;
                  "git.confirmSync" = false;
                  "editor.tabSize" = 2;
                  "editor.detectIndentation" = false;
                  "editor.minimap.enabled" = false;
                  "editor.autoClosingBrackets" = "never";
                  "editor.autoClosingQuotes" = "never";
                  "editor.autoClosingParentheses" = "never";
                  "files.autoSave" = "off";
                  "files.confirmDelete" = false;
                  "explorer.confirmDragAndDrop" = false;
                  "explorer.confirmDelete" = false;
                  "workbench.statusBar.visible" = true;
                  "workbench.colorTheme" = "Stylix";
                };
              };

              # -----------------------------------------------------------
              # waybar
              # -----------------------------------------------------------
              waybar = {
                enable = true;
                settings = [
                  {
                    layer = "top";
                    position = "bottom";

                    modules-left = ["niri/workspaces" "hyprland/workspaces" "idle_inhibitor"];

                    "niri/workspaces" = {
                      format = "{icon}";
                      "format-icons" = {
                        focused = "󰻀";
                        default = "";
                      };
                    };

                    "hyprland/workspaces" = {
                      format = "{icon}";
                      "format-icons" = {
                        focused = "󰻀";
                        default = "";
                      };
                    };

                    "idle_inhibitor" = {
                      format = "{icon}";
                      "format-icons" = {
                        activated = "";
                        deactivated = "";
                      };
                    };

                    modules-center = ["clock#1" "custom/divider" "clock#2"];

                    "clock#1" = {
                      format = "{:%H:%M:%S}";
                      tooltip = false;
                      interval = 1;
                    };

                    "custom/divider" = {
                      format = "|";
                      tooltip = false;
                    };

                    "clock#2" = {
                      format = "{:%m.%d.%y}";
                      tooltip = "true";
                      interval = 60;
                    };

                    modules-right = [
                      "pulseaudio"
                      "memory"
                      "cpu"
                      "disk"
                      "backlight"
                      "battery"
                      "tray"
                      "custom/power"
                    ];

                    "pulseaudio" = {
                      format = "{volume:2}% {icon}";
                      "format-bluetooth" = "{volume}% {icon}";
                      "format-muted" = "{volume}% 󰝟";
                      "format-icons" = {
                        headphones = "";
                        default = ["" ""];
                      };
                      "scroll-step" = 5;
                      "on-click" = "pamixer -t";
                      "on-click-right" = "pavucontrol";
                    };

                    "memory" = {
                      interval = 5;
                      format = "{}% ";
                    };

                    "cpu" = {
                      interval = 5;
                      format = "{usage:2}% ";
                    };

                    "disk" = {
                      interval = 5;
                      format = "{percentage_used:2}% ";
                      path = "/";
                    };

                    "backlight" = {
                      device = "amdgpu_bl1e";
                      format = "{percent}% {icon}";
                      "format-icons" = ["" "" "" "" "" "" "" "" ""];
                    };

                    "battery" = {
                      states = {
                        good = 95;
                        warning = 30;
                        critical = 15;
                      };
                      format = "{capacity}% {icon}";
                      "format-icons" = ["" "" "" "" ""];
                    };

                    "tray" = {};

                    "custom/power" = {
                      format = "⏻";
                      "on-click" = "wlogout";
                    };
                  }
                ];
                style = ''
                  @define-color background #303030;
                  @define-color text #E0E0E0;

                  * {
                    font-family: "Nerd Fonts Symbols Only", "Ariel", sans-serif;
                    font-size: 11px;
                  }

                  window#waybar {
                      background: @background;
                      color: @text;
                  }

                  #workspaces { background: @background; }
                  #workspaces button.focused { color: @text; }

                  #workspaces button {
                      padding: 0 2px;
                      color: @text;
                  }


                  #workspaces button:hover {
                      box-shadow: inherit;
                      text-shadow: inherit;
                  }
                  #workspaces button:hover {
                      background: @background;
                      padding: 0 3px;
                  }

                  #custom-divider {
                    background: @background;
                    color: @text;
                    padding: 0 0;
                  }

                  #idle_inhibitor,
                  #clock,
                  #pulseaudio,
                  #memory,
                  #cpu,
                  #disk,
                  #battery,
                  #tray,
                  #backlight,
                  #custom-power {
                      color: @text;
                      padding: 0 10px;
                  }
                '';
              };

              # -----------------------------------------------------------
              # niri config
              # -----------------------------------------------------------
              programs.niri.settings = {
                # general
                prefer-no-csd = true;
                hotkey-overlay.skip-at-startup = true;
                screenshot-path = "~/Pictures/Screenshots/Screenshot from %Y-%m-%d %H-%M-%S.png";
                # startup
                spawn-at-startup = [
                  {command = ["${lib.getExe pkgs.waybar}"];}
                  {command = ["${lib.getExe pkgs.nm-applet}"];}
                  {command = ["${lib.getExe pkgs.wlsunset}" "-T" "5200"];}
                  {command = ["${lib.getExe pkgs.swaybg}" "-i" "/home/goat/nixos-dotfiles/assets/wallpaper.png" "-m" "fill"];}
                ];
                # keybinds
                binds = let
                  spawn = command: {action.spawn = ["sh" "-c" ''${command}''];};
                  action = command: {action.spawn = ["sh" "-c" ''niri msg action ${command}''];};
                in {
                  # programs
                  "Mod+Return" = spawn "kitty";
                  "Mod+E" = spawn "thunar";
                  "Mod+Space" = spawn "fuzzel";
                  "Super+Shift+L" = spawn "swaylock";
                  "Super+Shift+P" = spawn "wlogout";
                  # media keys
                  "XF86AudioRaiseVolume" = spawn "wpctl set-volume @DEFAULT_AUDIO_SINK@ 0.1+";
                  "XF86AudioLowerVolume" = spawn "wpctl set-volume @DEFAULT_AUDIO_SINK@ 0.1-";
                  "XF86AudioMute" = spawn "wpctl set-mute @DEFAULT_AUDIO_SINK@ toggle";
                  "XF86AudioMicMute" = spawn "wpctl set-mute @DEFAULT_AUDIO_SOURCE@ toggle";
                  "XF86MonBrightnessUp" = spawn "brightnessctl --device=amdgpu_bl1 s 5%+";
                  "XF86MonBrightnessDown" = spawn "brightnessctl --device=amdgpu_bl1 s 5%-";
                  # screenshot
                  "Print" = spawn "screenshot";
                  "Ctrl+Print" = action "screenshot-screen";
                  "Alt+Print" = action "screenshot-window";
                  # window actions
                  "Mod+Q" = action "close-window";
                  "Ctrl+Alt+Delete" = action "quit";
                  "Mod+Left" = action "focus-column-left";
                  "Mod+Right" = action "focus-column-right";
                  "Mod+Up" = action "focus-workspace-up";
                  "Mod+Down" = action "focus-workspace-down";
                  "Mod+Shift+Left" = action "move-column-left";
                  "Mod+Shift+Right" = action "move-column-right";
                  "Mod+Shift+Up" = action "move-window-to-workspace-up";
                  "Mod+Shift+Down" = action "move-window-to-workspace-down";
                  # window presets
                  "Mod+R" = action "switch-preset-column-width";
                  "Mod+M" = action "maximize-column";
                  "Mod+Shift+M" = action "fullscreen-window";
                };
                # input
                input = {
                  keyboard.xkb.layout = "${def.layout}";
                  mouse.accel-speed = 1.0;
                  touchpad = {
                    tap = true;
                    dwt = true;
                    natural-scroll = true;
                    click-method = "clickfinger";
                  };
                  tablet.map-to-output = "eDP-1";
                  touch.map-to-output = "eDP-1";
                };
                # monitors
                outputs."DP-1" = {
                  enable = true;
                  mode.width = 1920;
                  mode.height = 1080;
                  position.x = 0;
                  position.y = 0;
                  mode.refresh = 165.001;
                };
                # layout n theming
                layout = {
                  gaps = 4;
                  border.width = 2;
                  always-center-single-column = false;
                };
                window-rules = [
                  {
                    geometry-corner-radius = let
                      r = 2.0;
                    in {
                      top-left = r;
                      top-right = r;
                      bottom-left = r;
                      bottom-right = r;
                    };
                    clip-to-geometry = true;
                  }
                ];
              };

              # -----------------------------------------------------------
              # nixvim
              # -----------------------------------------------------------
              nixvim = {
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
                    key = "<leader>bd";
                    action = "<cmd>bdelete<cr>";
                    options = {
                      desc = "Delete buffer";
                    };
                  }
                ];
                # plugins
                plugins = {
                  # icons
                  web-devicons.enable = true;
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
                  telescope.enable = true;
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
                  # tabs
                  bufferline.enable = true;
                  # cool bar
                  lualine.enable = true;
                  # fancy command window
                  noice.enable = true;
                  # nix expression support
                  nix.enable = true;
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
                  lazygit.enable = true;
                };
              };
            };
          }
        ];
      })
    ];
  };
}
