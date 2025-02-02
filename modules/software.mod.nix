{
  config,
  lib,
  pkgs,
  def,
  ...
}: let
  mk = import ./libs.nix {
    inherit lib;
    modules = [
      "wine"
      "steam"
      "wayland"
    ];
  };
in {
  options = mk.opts;

  config = lib.mkMerge [
    # -----------------------------------------------------------
    # globals - packages
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
  ];
}
