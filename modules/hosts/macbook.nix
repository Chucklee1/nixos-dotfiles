{inputs, ...}: {
  nix = [
    ({user, ...}: {
      # nix issue fix
      nix.settings.auto-optimise-store = false;
      nix.settings.trusted-users = ["@admin"];
      nix.settings.builders = "ssh-ng://builder@linux-builder aarch64-linux /etc/nix/builder_ed25519 4 - - -";

      nix.linux-builder.enable = true;
      nix.linux-builder.ephemeral = true;
      nix.linux-builder.systems = ["aarch64-linux"];

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
    # symlink nix & home manager apps to /Applications
    # lets spotlight or dmenu-mac finally acess nix apps
    ({lib, config, user, ...}: {
      system.activationScripts.applications.text = lib.mkForce ''
        appDirPrev="${config.users.users.${user}.home}/Applications/Home Manager Apps"
        appDirFinal="/Applications"
        for i in "$appDirPrev"/*; do
          [ -e "$i" ] || continue
          dest="$appDirFinal/$(basename "$i")"

          # Remove existing app or alias
          if [ -e "$dest" ]; then
            rm -rf "$dest"
          fi

          # Copy new app/alias
          cp -R "$i" "$appDirFinal"/
        done
      '';
    })
    # need to manually include nerd-font-symbols
    ({pkgs, ...}: {fonts.packages = [pkgs.nerd-fonts.symbols-only];})
    {
      # defaults
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

      system.keyboard.enableKeyMapping = true;
      system.keyboard.remapCapsLockToControl = true;

      system.defaults.finder = {
        AppleShowAllExtensions = true;
        AppleShowAllFiles = true;
        ShowPathbar = true;
      };

      # misc
      system.defaults.CustomUserPreferences = {
        "com.apple.desktopservices".DSDontWriteNetworkStores = true;
        "com.apple.desktopservices".DSDontWriteUSBStores = true;
        "com.apple.AdLib".allowApplePersonalizedAdvertising = false;
      };
    }
    inputs.nix-homebrew.darwinModules.nix-homebrew
    ({user, ...}: {
      nix-homebrew = {
        inherit user; # User owning the Homebrew prefix
        enable = true;
        # autoMigrate = true;
        enableRosetta = true;
        # Declarative tap management
        taps = {
          "homebrew/homebrew-core" = inputs.homebrew-core;
          "homebrew/homebrew-cask" = inputs.homebrew-cask;
          "homebrew/homebrew-bundle" = inputs.homebrew-bundle;
          "railwaycat/homebrew-emacsmacport" = inputs.homebrew-emacsmacport;
        };
        # With mutableTaps disabled, taps can no longer be added imperatively with `brew tap`.
        mutableTaps = false;
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

        taps = builtins.attrNames config.nix-homebrew.taps; #Align homebrew taps config with nix-homebrew

        caskArgs.no_quarantine = true;
        brews = ["syncthing"];
        casks = [
          "ghostty"
          "hammerspoon"
          "krita"
          "utm"
          "zoom"
        ];
      };
    })
  ];
  home = [
    # ghostty
    {
      home.file.".config/ghostty/config".text = ''
        background-opacity = 0.8
        background-blur = 30
        macos-titlebar-style = "tabs"
      '';
    }
  ];
}
