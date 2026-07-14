{
  inputs,
  self,
  mod,
  ...
}:
with mod; {
  system = "aarch64-darwin";
  type = "darwin";
  user = "goat";
  modules = [
    services.tailscale

    programs.nixvim
    programs.git
    programs.yazi
    programs.kitty

    software.dev
    software.qol
    software.texlive
    software.java
    software.rust

    games.prismLauncher

    system.home
    system.pkgconfig

    shell.variables
    shell.fish
  ];
  extraConfig = [
    ({user, ...}: {
      # nix issue fix
      nix.settings.auto-optimise-store = false;

      nixpkgs.overlays = [
        (import self.inputs.emacs-overlay)
        self.overlays.emacs
      ];

      # general
      system.stateVersion = 6;
      system.primaryUser = user;

      # shut up bash
      environment.variables = {
        BASH_SILENCE_DEPRECATION_WARNING = "1"; # fix for macOS
      };

      # user
      users.knownUsers = [user];
      users.users.${user} = {
        name = user;
        uid = 501;
        home = "/Users/${user}";
        ignoreShellProgramCheck = true;
      };
    })
    ({
      lib,
      pkgs,
      ...
    }: {
      # need to manually include nerd-font-symbols
      fonts.packages = [pkgs.nerd-fonts.symbols-only];
      environment.systemPackages = with pkgs; [
        coreutils-prefixed
        feishin
        mpv
        emacs-macport
      ];

      # symlink nix & home manager apps to /Applications
      # lets spotlight or dmenu-mac finally acess nix apps
      system.activationScripts.applications.text = let
        MAC_APPDIR = "/Applications";
        NIX_APPDIR = "/run/current-system/Applications";
      in
        lib.mkForce ''
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

        taps = builtins.attrNames config.nix-homebrew.taps; # Align homebrew taps config with nix-homebrew

        brews = [
          "syncthing"
          "gcc"
        ];
        casks = [
          "blackhole-2ch"
          "krita"
          "musescore"
          "musicbrainz-picard"
          "zen"
          "zoom"
        ];
      };
    })
    # not dealing with nixos/darwin stylix conflicts
    inputs.stylix.darwinModules.stylix
    ({pkgs, ...}: {
      stylix = {
        enable = true;
        autoEnable = true;
        homeManagerIntegration.autoImport = true;
        image = "${self}/assets/wallpaper/nordest.png";
        base16Scheme = "${pkgs.base16-schemes}/share/themes/nord.yaml";
        polarity = "dark";

        fonts = {
          monospace.package = pkgs.nerd-fonts.jetbrains-mono;
          monospace.name = "JetBrainsMono Nerd Font Mono";
          sansSerif.package = pkgs.noto-fonts-cjk-sans;
          sansSerif.name = "Noto Sans CJK";
          serif.package = pkgs.noto-fonts-cjk-serif;
          serif.name = "Noto Serif CJK";

          sizes = {
            applications = 14;
            terminal = 14;
            desktop = 14;
            popups = 12;
          };
        };
      };
    })
  ];
}
