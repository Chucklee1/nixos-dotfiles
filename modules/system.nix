{inputs, ...}: {
  global.nix = [
    ({lib, ...}: {
      nix.settings.experimental-features = "nix-command flakes";
      nix.gc = {
        automatic = lib.mkDefault true;
        options = lib.mkDefault "--delete-older-than 7d";
      };
      nixpkgs.config.allowUnfree = true;
    })
    ({
      config,
      user,
      ...
    }: {
      home-manager.useGlobalPkgs = true;
      home-manager.backupFileExtension = "backup";
      home-manager.users.${user} = {
        home = {
          stateVersion = "24.05"; # DO NOT CHANGE
          username = user;
          homeDirectory = config.users.users.${user}.home;
        };
        imports = config._module.args.homeMods;
      };
    })
  ];

  metal.nix = [
    ({
      lib,
      user,
      machine,
      ...
    }: {
      # ---- boot ----
      boot.loader = {
        efi.canTouchEfiVariables = true;
        grub = {
          enable = true;
          efiSupport = true;
          device = "nodev";
        };
      };

      # ---- system ----
      system.stateVersion = "24.05";
      networking = {
        useDHCP = lib.mkDefault true;
        hostName = "${user}-${machine}";
        networkmanager.enable = true;
      };
      i18n.defaultLocale = "en_CA.UTF-8";
      time.timeZone = "America/Vancouver";

      # ---- user ----
      users.users.goat = {
        name = "goat";
        isNormalUser = true;
        extraGroups = [
          "media"
          "wheel"
          "input"
          "networkmanager"
          "gamemode"
          "libvirtd"
        ];
      };
    })
  ];

  linux.nix = [inputs.home-manager.nixosModules.home-manager];

  macbook.nix = [
    inputs.home-manager.darwinModules.home-manager
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

      system.defaults.finder = {
        AppleShowAllExtensions = true;
        AppleShowAllFiles = true;
        ShowPathbar = true;
      };

      system.defaults.NSGlobalDomain = {
        AppleInterfaceStyle = "Dark";
        AppleInterfaceStyleSwitchesAutomatically = false;
        NSDocumentSaveNewDocumentsToCloud = false; # do not save to icloud by default
        # no autocorrections
        NSAutomaticCapitalizationEnabled = false;
        NSAutomaticSpellingCorrectionEnabled = false;
      };

      # misc
      system.defaults.CustomUserPreferences = {
        "com.apple.desktopservices".DSDontWriteNetworkStores = true;
        "com.apple.desktopservices".DSDontWriteUSBStores = true;
        "com.apple.AdLib".allowApplePersonalizedAdvertising = false;
      };

      # Add ability to used TouchID for sudo authentication
      security.pam.services.sudo_local.touchIdAuth = true;
    }
  ];
}
