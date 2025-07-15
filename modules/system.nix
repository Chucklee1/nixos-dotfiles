{inputs, ...}: let
  linuxNix = [
    # ---- system ----
    inputs.home-manager.nixosModules.home-manager
    ({
      lib,
      user,
      machine,
      ...
    }: {
      # boot
      boot.loader = {
        efi.canTouchEfiVariables = true;
        grub = {
          enable = true;
          efiSupport = true;
          device = "nodev";
        };
      };

      # general
      system.stateVersion = "24.05";
      networking = {
        useDHCP = lib.mkDefault true;
        hostName = "${user}-${machine}";
        networkmanager.enable = true;
      };
      i18n.defaultLocale = "en_CA.UTF-8";
      time.timeZone = "America/Vancouver";

      # user
      users.users.${user} = {
        name = "${user}";
        isNormalUser = true;
        extraGroups = [
          "wheel"
          "networkmanager"
          "audio"
          "video"
          "input"
          "gamemode"
          "libvirtd"
        ];
      };
    })
    # ---- higher-level drivers ----
    ({pkgs, ...}: {
      # gpu
      hardware.graphics = {
        enable = true;
        enable32Bit = true;
        extraPackages = with pkgs; [
          vulkan-tools
          vulkan-loader
          libvdpau-va-gl
        ];
      };

      # audio
      security.rtkit.enable = true;
      services.pipewire = {
        enable = true;
        alsa.enable = true;
        alsa.support32Bit = true;
        pulse.enable = true;
      };

      # bluetooth
      hardware.bluetooth.enable = true;
      services.blueman.enable = true;

      # ssh
      services.openssh = {
        enable = true;
        settings = {
          PasswordAuthentication = false;
          PermitRootLogin = "prohibit-password";
        };
      };

      # misc
      services = {
        printing.enable = true;
        fstrim.enable = true;
        tumbler.enable = true;
        gvfs.enable = true;
      };
    })
  ];
in {
  nix.global = [
    ({
      lib,
      config,
      user,
      ...
    }: {
      # pkg conf
      nix.settings.experimental-features = "nix-command flakes";
      nix.gc = {
        automatic = lib.mkDefault true;
        options = lib.mkDefault "--delete-older-than 7d";
      };
      nixpkgs.config.allowUnfree = true;
      # home manager
      home-manager.useGlobalPkgs = true;
      home-manager.users.${user}.imports = config._module.args.homeMods;
    })
  ];
  nix.desktop =
    linuxNix
    ++ [
      # gpu
      ({
        lib,
        config,
        ...
      }: {
        nixpkgs.config.nvidia.acceptLicense = true;
        services.xserver.videoDrivers = ["nvidia"];
        hardware.nvidia = {
          modesetting.enable = true;
          package = config.boot.kernelPackages.nvidiaPackages.beta;
          videoAcceleration = true;
          open = false;
        };
        environment.variables =
          {
            LIBVA_DRIVER_NAME = "nvidia";
            NVD_BACKEND = "direct";
          }
          // lib.mkIf config.programs.niri.enable {
            GBM_BACKEND = "nvidia-drm";
            __GLX_VENDOR_LIBRARY_NAME = "nvidia";
          };
      })
      # tablet support
      {
        hardware.uinput.enable = true;
        programs.weylus.enable = true;
        services.udev.extraRules = ''
          KERNEL=="uinput", MODE="0660", GROUP="uinput", OPTIONS+="static_node=uinput"
        '';
      }
    ];

  nix.macbook = [
    inputs.home-manager.darwinModules.home-manager
    ({
      pkgs,
      user,
      ...
    }: {
      # nix issue fix
      nix.settings = {auto-optimise-store = false;};
      system = {
        # general
        stateVersion = 6;
        primaryUser = "${user}";
      };
      system.defaults = {
        WindowManager.StandardHideDesktopIcons = true;
        dock = {
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

        finder = {
          AppleShowAllExtensions = true;
          AppleShowAllFiles = true;
          ShowPathbar = true;
        };

        NSGlobalDomain = {
          AppleInterfaceStyle = "Dark";
          AppleInterfaceStyleSwitchesAutomatically = false;
          NSDocumentSaveNewDocumentsToCloud = false; # do not save to icloud by default
        };

        # misc
        CustomUserPreferences = {
          "com.apple.desktopservices".DSDontWriteNetworkStores = true;
          "com.apple.desktopservices".DSDontWriteUSBStores = true;
          "com.apple.AdLib".allowApplePersonalizedAdvertising = false;
        };
      };

      # Add ability to used TouchID for sudo authentication
      security.pam.services.sudo_local.touchIdAuth = true;

      # user
      users.knownUsers = ["goat"];
      users.users.${user} = {
        uid = 501;
        name = "${user}";
        home = "/Users/${user}";
        shell = pkgs.bash;
        ignoreShellProgramCheck = true;
      };
    })
    {
      services.yabair.enable = true;
      # csrutils disable
      # sudo nvram boot-args=-arm64e_preview_abi
      services.yabai.enableScriptingAddition = true;
      services.skhd.enable = true;
      services.jankyborders.enable = true;
    }
  ];
  home.global = [
    ({
      ifSys,
      user,
      ...
    }: let
      homeDir = ifSys.darwin "/Users" "/home";
    in {
      home = {
        stateVersion = "24.05"; # DO NOT CHANGE
        username = user;
        homeDirectory = "${homeDir}/${user}";
      };
    })
  ];
}
