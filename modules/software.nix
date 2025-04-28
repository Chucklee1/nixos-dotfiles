{user, ...}: {
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
        ffmpeg
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
            thunar-media-tags-plugin
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
        picard
        qbittorrent
        musescore
        muse-sounds-manager
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
        firefox = {
          enable = true;
          enableGnomeExtensions = true;
          policies = {
            enableTrackingProtection = {
              Value = true;
              Locked = true;
              Cryptomining = true;
              Fingerprinting = true;
            };
            DisableTelemetry = true;
            DisablePocket = true;
            DisableFirefoxStudies = true;
            DisableFirefoxAccounts = true;
            DisableFirefoxScreenshots = true;
            DisableSafeMode = true;
          };
          profiles.${user} = {
            name = user;
            search.default = "ddg";
            settings."browser.startup.homepage" = "";
          };
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
            flake = "--impure --show-trace --flake $HOME/nixos-dotfiles";
          in {
            # nix - system installation
            disko-generate = ''sudo nix --experimental-features "nix-command flakes" run github:nix-community/disko -- --mode disko /tmp/disko.nix'';
            show-hardware = "sudo nixos-generate-config ${flake}";
            install-mnt = "sudo nixos-install --root /mnt ${flake}";
            # nix - general
            cg = "nix-collect-garbage";
            update-flake = "nix flake update --flake $HOME/nixos-dotfiles";
            rebuild-desktop = "sudo nixos-rebuild switch ${flake}#desktop";
            rebuild-macbook = "sudo nixos-rebuild switch  ${flake}#macbook";
            # tools - git
            clone-flake = ''
              if [ -e /home/goat/.ssh/id_ed25519.pub ]; then
                echo "found ssh key, cloning with ssh..."
                git clone git@github.com:Chucklee1/nixos-dotfiles
              else
                echo "no ssh key found, trying tls..."
                git clone https://github.com/Chucklee1/nixos-dotfiles
              fi
            '';
            # tools - ffmpeg
            wav-to-flac = ''
              for i in *.wav; do ffmpeg -i "$i" -c:a flac "$i%.*.flac"; done
              for i in *.wav; do rm "$i"; done
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
    # games
    ({pkgs, ...}: {
      environment.systemPackages = with pkgs; [
        osu-lazer-bin
        prismlauncher
      ];
      programs.steam = {
        enable = true;
        protontricks.enable = true;
        gamescopeSession.enable = true;
        extraCompatPackages = [pkgs.proton-ge-bin];
        remotePlay.openFirewall = true;
        dedicatedServer.openFirewall = true;
        localNetworkGameTransfers.openFirewall = true;
      };
    })

    # wine
    ({pkgs, ...}: {
      environment.systemPackages = with pkgs; [
        zenity
        samba
        wine
        wineWowPackages.stagingFull
        winetricks
      ];
    })
  ];
}
