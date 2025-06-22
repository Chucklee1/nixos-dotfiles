{inputs, ...}: let
  linuxNix = [
    # ---- system ----
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
          "libvirtd"
        ];
      };
    })
    inputs.home-manager.nixosModules.home-manager
    ({user, ...}: {
      home-manager.users.${user}.home = {
        stateVersion = "24.05"; # DO NOT CHANGE
        username = user;
        homeDirectory = "/home/${user}";
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
      config,
      system,
      user,
      ...
    }: {
      # pkg conf
      nix.settings.experimental-features = "nix-command flakes";
      nixpkgs = {
        hostPlatform = "${system}";
        config.allowUnfree = true;
      };
      # home manager
      home-manager.useGlobalPkgs = true;
      home-manager.users.${user}.imports = config._module.args.homeMods;
    })
  ];
  nix.laptop =
    linuxNix
    ++ [
      {networking.networkmanager.wifi.macAddress = "random";}
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

  nix.macbook = let
    user = "goat";
  in [
    inputs.home-manager.darwinModules.home-manager
    ({pkgs, ...}: {
      # system cfg
      system.stateVersion = 6;
      system.primaryUser = "goat";

      # user
      users.knownUsers = ["goat"];
      users.users.${user} = {
        uid = 501;
        name = "${user}";
        home = "/Users/${user}";
        shell = pkgs.bash;
        ignoreShellProgramCheck = true;
      };
      home-manager.users.${user}.home.stateVersion = "25.05";
      # shell
      programs.bash = {
        completion.enable = true;
        enable = true;
        interactiveShellInit = "";
      };
    })
    # homebrew
    {
      homebrew = {
        caskArgs.no_quarantine = true;
        enable = true;
        #brews = [];
        casks = [
          "kitty"
          "librewolf"
          "prismlauncher"
        ];
      };
    }
  ];
}
