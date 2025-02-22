let
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
    ({
      lib,
      pkgs,
      modulesPath,
      ...
    }: {
      imports = [(modulesPath + "/installer/scan/not-detected.nix")];
      swapDevices = [];
      networking.useDHCP = lib.mkDefault true;
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
  ];

  nix.desktop = [
    (mkFs "/" "/dev/disk/by-uuid/96c41aaf-846f-47b1-8319-eed5a3a32294" "ext4" null)
    (mkFs "/boot" "/dev/disk/by-uuid/75D4-A9F7" "vfat" ["fmask=0022" "dmask=0022"])
    (mkFs "/media/goat/BLUE_SATA" "/dev/disk/by-uuid/a6ffb4f9-049c-49a1-8b5f-1aca1b8dca08" "ext4")

    # general hardware
    ({lib, ...}: {
      boot = {
        initrd.availableKernelModules = ["nvme" "xhci_pci" "ahci" "usb_storage" "usbhid" "sd_mod"];
        initrd.kernelModules = [];
        kernelModules = ["kvm-amd"];
        extraModulePackages = [];
        supportedFilesystems = ["ntfs"];
        loader.grub.useOSProber = true;
      };

      networking.interfaces.enp7s0.useDHCP = lib.mkDefault true;
      networking.interfaces.wlp6s0.useDHCP = lib.mkDefault true;
    })
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
    {
      hardware.uinput.enable = true;
      programs.weylus.enable = true;
      services.udev.extraRules = ''
        KERNEL=="uinput", MODE="0660", GROUP="uinput", OPTIONS+="static_node=uinput"
      '';

      networking.firewall = {
        enable = true;
        allowedTCPPorts = [1701 9001];
        allowedUDPPortRanges = [
          {
            from = 4000;
            to = 4007;
          }
          {
            from = 8000;
            to = 8010;
          }
        ];
      };
    }
  ];

  nix.laptop = [
    (mkFs "/" "/dev/disk/by-uuid/5d6d6313-52a3-438e-bc02-53dc6ea56c1a" "ext4" null)
    (mkFs "/boot" "/dev/disk/by-uuid/0E8B-9EFC" "vfat" ["fmask=0077" "dmask=0077"])

    ({lib, ...}: {
      boot = {
        initrd.availableKernelModules = ["nvme" "xhci_pci" "uas" "usb_storage" "sd_mod"];
        initrd.kernelModules = [];
        kernelModules = ["kvm-amd"];
        extraModulePackages = [];
      };

      networking.interfaces.wlp2s0.useDHCP = lib.mkDefault true;
      services.xserver.videoDrivers = ["amdgpu"];
      hardware.amdgpu.amdvlk.enable = true;
    })
  ];
}
