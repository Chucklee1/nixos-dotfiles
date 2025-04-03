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

  setCPU = cpu:
    if cpu == "amd"
    then {
      boot.kernelModules = ["kvm-amd"];
      hardware.cpu.amd.updateMicrocode = true;
    }
    else if cpu == "intel"
    then {hardware.cpu.intel.updateMicrocode = true;}
    else {};

  setGPU = gpu:
    if gpu == "nvidia"
    then
      ({config, ...}:
        {
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
          };
        }
        // (
          if config.programs.niri.enable == true
          then {
            environment.variables = {
              GBM_BACKEND = "nvidia-drm";
              __GLX_VENDOR_LIBRARY_NAME = "nvidia";
            };
          }
          else {}
        ))
    else if gpu == "amd"
    then {
      services.xserver.videoDrivers = ["amdgpu"];
      hardware.amdgpu.amdvlk.enable = true;
    }
    else {};

  setAllLazy = swapDevices: cpu: gpu: {inherit swapDevices;} // (setCPU cpu) // (setGPU gpu);
in {
  nix.global = [
    ({
      pkgs,
      modulesPath,
      ...
    }: {
      imports = [(modulesPath + "/installer/scan/not-detected.nix")];

      boot.initrd.systemd.enable = true; # force systemd to load early
      boot.loader.efi.canTouchEfiVariables = true;
      boot.loader.grub = {
        enable = true;
        efiSupport = true;
        device = "nodev";
      };

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
    (mkFs "/media/goat/BLUE_SATA" "/dev/disk/by-uuid/a6ffb4f9-049c-49a1-8b5f-1aca1b8dca08" "ext4" null)
    (setAllLazy [] "amd" "nvidia")
    {
      boot = {
        initrd.availableKernelModules = ["nvme" "xhci_pci" "ahci" "usb_storage" "usbhid" "sd_mod"];
        initrd.kernelModules = [];
        extraModulePackages = [];
        supportedFilesystems = ["ntfs"];
      };
    }
  ];

  nix.laptop = [
    (mkFs "/" "/dev/disk/by-uuid/5d6d6313-52a3-438e-bc02-53dc6ea56c1a" "ext4" null)
    (mkFs "/boot" "/dev/disk/by-uuid/0E8B-9EFC" "vfat" ["fmask=0077" "dmask=0077"])
    (setAllLazy [] "amd" "amd")
    {
      boot = {
        initrd.availableKernelModules = ["nvme" "xhci_pci" "uas" "usb_storage" "sd_mod"];
        initrd.kernelModules = [];
        extraModulePackages = [];
      };
    }
  ];
}
