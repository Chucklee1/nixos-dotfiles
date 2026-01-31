{inputs, mod, ...}: with mod; {
  system = "aarch64-darwin";
  builder = inputs.nix-darwin.lib.darwinSystem;
  user = "goat";
  modules = [
    net.tailscale

    programs.emacs programs.prismLauncher
    programs.git programs.yazi

    software.dev software.qol software.texlive software.java software.rust

    system.home system.pkgconfig

    shell.variables shell.zsh

    theming.stylix theming.themes.nord
  ];
  extraConfig = [
    ({user, ...}: {
      # nix issue fix
      nix.settings.auto-optimise-store = false;

      # general
      system.stateVersion = 6;
      system.primaryUser = user;

      # user
      users.knownUsers = [user];
      users.users.${user} = {
        name = user;
        uid = 501;
        home = "/Users/${user}";
        ignoreShellProgramCheck = true;
      };
    })
    # need to manually include nerd-font-symbols
    ({lib, pkgs, ...}: {
      fonts.packages = [pkgs.nerd-fonts.symbols-only];
      environment.systemPackages = [pkgs.feishin pkgs.mpv];

      # symlink nix & home manager apps to /Applications
      # lets spotlight or dmenu-mac finally acess nix apps
      system.activationScripts.applications.text = let
      MAC_APPDIR="/Applications";
      NIX_APPDIR="/run/current-system/Applications";
      in lib.mkForce ''
        printf "\e[0;33m%s\e[m\n" "setting up application symlinks..."
        for app in ${NIX_APPDIR}/*; do
          rm -rf "${MAC_APPDIR}/$'''{app##*/}"
          ln -sf "$app" "${MAC_APPDIR}";
          printf "\e[0;32m%s $app\e[m\n" "sylinked"
        done
        printf "\e[0;32m%s\e[m\n" "symlinking finished!"
      '';

      # defaults - desktop
      system.defaults.WindowManager.StandardHideDesktopIcons = true;
      system.defaults.dock = {
        autohide = true;
        dashboard-in-overlay = true; # Don't show dashboard as a space
        mru-spaces = false; # Don't rearrange spaces based on most recently used
        show-recents = false; # don't show recent apps
        static-only = false; # show only running apps
        # Disable all hot corners
        wvous-tl-corner = 1;
        wvous-tr-corner = 1;
        wvous-bl-corner = 1;
        wvous-br-corner = 1;
      };

      # defaults - keyboard
      system.keyboard.enableKeyMapping = true;
      system.keyboard.remapCapsLockToControl = true;

      # defaults - finder
      system.defaults.finder = {
        AppleShowAllExtensions = true;
        AppleShowAllFiles = true;
        ShowPathbar = true;
      };

      # defaults - misc
      system.defaults.CustomUserPreferences = {
        "com.apple.desktopservices".DSDontWriteNetworkStores = true;
        "com.apple.desktopservices".DSDontWriteUSBStores = true;
        "com.apple.AdLib".allowApplePersonalizedAdvertising = false;
      };
    })
    inputs.nix-homebrew.darwinModules.nix-homebrew
    ({user, ...}: {
      nix-homebrew = {
        inherit user; # User owning the Homebrew prefix
        enable = true;
        autoMigrate = true;
        enableRosetta = true;
        # Declarative tap management
        taps = {
          "homebrew/homebrew-core" = inputs.homebrew-core;
          "homebrew/homebrew-cask" = inputs.homebrew-cask;
          "homebrew/homebrew-bundle" = inputs.homebrew-bundle;
        };
        # With mutableTaps disabled, taps can no longer be added imperatively with `brew tap`.
        mutableTaps = true;
      };
    })
    ({config, ...}: {
      homebrew = {
        enable = true;
        onActivation = {
          autoUpdate = true;
          upgrade = true;
          cleanup = "zap";
        };

        taps = builtins.attrNames config.nix-homebrew.taps; # Align homebrew taps config with nix-homebrew

        caskArgs.no_quarantine = true;
        brews = [
          "syncthing"
        ];
        casks = [
          "blackhole-2ch"
          "ghostty"
          "krita"
          "musicbrainz-picard"
          "zen"
          "zoom"
        ];
      };
    })
    # ghostty
    ({user, ...}: {
      home-manager.users.${user} = {
        home.file.".config/ghostty/config".text = ''
          background-opacity = 0.8
          background-blur = 30
          macos-titlebar-style = "tabs"
        '';
      };
    })
  ];
}
