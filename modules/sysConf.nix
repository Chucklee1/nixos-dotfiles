{inputs, ...}: let
  mkFs = path: device: fsType: options: {
    fileSystems.${path} =
      {inherit device fsType;}
      // (
        if options == null
        then {}
        else {inherit options;}
      );
  };
in {
  nix.global = [
    inputs.home-manager.nixosModules.home-manager
    ({modulesPath, ...}: {
      imports = [(modulesPath + "/installer/scan/not-detected.nix")];
      swapDevices = [];
    })
    ({
      lib,
      config,
      ...
    }: {
      # boot
      boot = {
        initrd.systemd.enable = true; # force systemd to load early
        loader = {
          efi.canTouchEfiVariables = true;
          grub = {
            enable = true;
            efiSupport = true;
            device = "nodev";
          };
        };
      };

      # system options
      system.stateVersion = "24.05";
      networking = {
        useDHCP = lib.mkDefault true;
        networkmanager.enable = true;
        hostName = "goat";
      };
      i18n.defaultLocale = "en_CA.UTF-8";
      time.timeZone = "America/Vancouver";
      console = {
        earlySetup = true;
        keyMap = "us";
      };

      # nix options
      nixpkgs = {
        hostPlatform = lib.mkDefault "x86_64-linux";
        config.allowUnfree = true;
      };
      nix.settings = {
        auto-optimise-store = true;
        experimental-features = ["nix-command" "flakes"];
      };

      # user
      users.users."goat" = {
        name = "goat";
        isNormalUser = true;
        extraGroups = [
          "wheel"
          "networkmanager"
          "audio"
          "video"
        ];
      };

      home-manager.users."goat" = {
        home = {
          stateVersion = "24.05"; # DO NOT CHANGE
          username = "${config.users.users."goat".name}";
          homeDirectory = "/home/goat";
        };
        imports = config._module.args.homeMods;
      };
    })
    # hardware
    ({
      lib,
      pkgs,
      ...
    }: {
      hardware.cpu.amd.updateMicrocode = lib.mkDefault true;
      hardware.graphics = {
        enable = true;
        enable32Bit = true;
        extraPackages = with pkgs; [
          vulkan-tools
          vulkan-loader
          libvdpau-va-gl
          ffmpeg
        ];
      };
    })
    # services
    {
      security.polkit.enable = true;

      # audio
      security.rtkit.enable = true;
      services.pipewire = {
        enable = true;
        alsa.enable = true;
        alsa.support32Bit = true;
        pulse.enable = true;
      };

      # bluetooth
      hardware = {
        bluetooth.enable = true;
        bluetooth.powerOnBoot = true;
      };
      services.blueman.enable = true;

      # misc
      services = {
        displayManager.ly.enable = true;
        printing.enable = true;
        fstrim.enable = true;
        tumbler.enable = true;
        gvfs.enable = true;
      };
    }
    # net related
    {
      # ssh
      services.openssh = {
        enable = true;
        settings = {
          PasswordAuthentication = false;
          PermitRootLogin = "prohibit-password";
        };
      };
    }
  ];

  nix.desktop = [
    (mkFs "/" "/dev/disk/by-uuid/96c41aaf-846f-47b1-8319-eed5a3a32294" "ext4" null)
    (mkFs "/boot" "/dev/disk/by-uuid/75D4-A9F7" "vfat" ["fmask=0022" "dmask=0022"])
    (mkFs "/media/goat/BLUE_SATA" "/dev/disk/by-uuid/a6ffb4f9-049c-49a1-8b5f-1aca1b8dca08" "ext4" null)

    # general hardware
    {
      boot = {
        initrd.availableKernelModules = ["nvme" "xhci_pci" "ahci" "usb_storage" "usbhid" "sd_mod"];
        initrd.kernelModules = [];
        kernelModules = ["kvm-amd"];
        extraModulePackages = [];
        supportedFilesystems = ["ntfs"];
      };
    }
    # nvidia
    ({config, ...}: {
      nixpkgs.config.nvidia.acceptLicense = true;
      services.xserver.videoDrivers = ["nvidia"];
      hardware.nvidia = {
        modesetting.enable = true;
        package = config.boot.kernelPackages.nvidiaPackages.beta;
        videoAcceleration = true;
        open = false;
      };
      environment.variables = {
        LIBVA_DRIVER_NAME = "nvidia";
        NVD_BACKEND = "direct";
        # wayland
        GBM_BACKEND = "nvidia-drm";
        __GLX_VENDOR_LIBRARY_NAME = "nvidia";
      };
    })
    # tablet support
    {
      users.users."goat".extraGroups = ["uinput"];
      hardware.uinput.enable = true;
      programs.weylus.enable = true;
      services.udev.extraRules = ''
        KERNEL=="uinput", MODE="0660", GROUP="uinput", OPTIONS+="static_node=uinput"
      '';
    }
  ];

  nix.laptop = [
    (mkFs "/" "/dev/disk/by-uuid/5d6d6313-52a3-438e-bc02-53dc6ea56c1a" "ext4" null)
    (mkFs "/boot" "/dev/disk/by-uuid/0E8B-9EFC" "vfat" ["fmask=0077" "dmask=0077"])

    {
      boot = {
        initrd.availableKernelModules = ["nvme" "xhci_pci" "uas" "usb_storage" "sd_mod"];
        initrd.kernelModules = [];
        kernelModules = ["kvm-amd"];
        extraModulePackages = [];
      };

      services.xserver.videoDrivers = ["amdgpu"];
      hardware.amdgpu.amdvlk.enable = true;
    }
  ];
}
