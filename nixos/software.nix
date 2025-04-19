{
  nix.global = [
    ({pkgs, ...}: {
      environment.systemPackages = with pkgs; [
        # dependancies
        libnotify
        libsecret
        # cli
        ripgrep
        pciutils
        # web/net
        wget
        git
        curl
        # media/files
        file-roller
        p7zip
        mpv
        pavucontrol
        v4l-utils
      ];

      # programs
      programs = {
        dconf.enable = true;
        xfconf.enable = true;
        thunar = {
          enable = true;
          plugins = with pkgs.xfce; [
            thunar-archive-plugin
            thunar-volman
          ];
        };
      };
    })
  ];
  home.global = [
    ({
      lib,
      config,
      pkgs,
      ...
    }: {
      home.packages = with pkgs; [
        # dev tools
        openai
        python3
        gnumake
        gdb
        gcc
        # apps
        tenacity
        gimp
        spotdl
        picard
        feishin
        qbittorrent
        musescore
        muse-sounds-manager
        libreoffice
        logisim-evolution
      ];

      programs = {
        # diagnostics
        btop.enable = true;
        mangohud.enable = true;
        #git
        git = {
          enable = true;
          userEmail = "kermitthefrog@kakao.com";
          userName = "Chucklee1";
        };

        # browser
        chromium = {
          enable = true;
          package = pkgs.ungoogled-chromium;
        };

        # shell
        kitty = {
          enable = true;
          settings = {
            confirm_os_window_close = 0;
            hide_window_decorations = true;
            tab_bar_edge = "top";
            tab_bar_style = lib.mkForce "slant";
          };
        };

        bash = {
          enable = true;
          shellAliases = let
            flake = "--impure --show-trace --flake github:Chucklee1/nixos-dotfiles";
          in {
            # nix - system installation
            disko-generate = ''sudo nix --experimental-features "nix-command flakes" run github:nix-community/disko -- --mode disko /tmp/disko.nix'';
            show-hardware = "sudo nixos-generate-config ${flake}";
            install-mnt = "sudo nixos-install --root /mnt ${flake}";
            # nix - general
            cg = "nix-collect-garbage";
            update-flake = "nix flake update --flake $HOME/nixos-dotfiles";
            rebuild-desktop = "sudo nixos-rebuild switch ${flake}#desktop";
            rebuild-laptop = "sudo nixos-rebuild switch  ${flake}#laptop";
            # git
            sshkey-init = "ssh-keygen -t ed25519 -C ${config.programs.git.userEmail}";
            open-sops = ''nix-shell -p sops --run "sops $HOME/nixos-dotfiles/secrets.yaml"'';
            clone-flake = ''
              if [ -e /home/goat/.ssh/id_ed25519.pub ]; then
                echo "found ssh key, cloning with ssh..."
                git clone git@github.com:Chucklee1/nixos-dotfiles
              else
                echo "no ssh key found, trying tls..."
                git clone https://github.com/Chucklee1/nixos-dotfiles
              fi
            '';
          };
        };
        oh-my-posh = {
          enable = true;
          enableBashIntegration = true;
          useTheme = "pure";
        };
      };
    })
  ];

  nix.desktop = [
    ({pkgs, ...}: {
      environment.systemPackages = with pkgs; [
        osu-lazer-bin
        prismlauncher
      ];
    })
    {
      programs.steam = {
        enable = true;
        remotePlay.openFirewall = true;
        dedicatedServer.openFirewall = true;
        localNetworkGameTransfers.openFirewall = true;
      };
      environment.variables.STEAM_EXTRA_COMPAT_TOOLS_PATHS = "~/.steam/root/compatibilitytools..d";
    }
    # wine
    ({pkgs, ...}: {
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
  ];
}
