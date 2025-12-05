let
  DEV = {
    WD = "/dev/disk/by-uuid/ff111615-41d4-4836-8adc-c9374e08bee9";
    EFI = "/dev/disk/by-uuid/5F4E-A7BB"; # on same device as WD
    EVO = "/dev/disk/by-uuid/5a5dcb04-31cb-4fae-8fbe-1e8e83a83501";
  };
  # passing fsType first to allow for specifying mkfs.fsType when calling
  # if there are no options, pass an empty list
  mkfs' = fsType: path: device: options: {
    fileSystems."${path}" = {
      inherit device fsType options;
    };
  };

  mkfs = {
    btrfs = path: device: options: mkfs' "btrfs";
    ext4 = path: device: options: mkfs' "ext4";
    vfat = path: device: options: mkfs' "vfat";
  };
in {
  nix = [
    (mkfs.vfat  "/boot/EFI"          DEV.EFI ["fmask=0022" "dmask=0022"])
    (mkfs.btrfs "/"                  DEV.WD  ["subvol=WD/nixos" "relatime" "compress=zstd"])
    (mkfs.btrfs "/boot"              DEV.WD  ["subvol=WD/nixos/boot" "noatime"])
    (mkfs.btrfs "/nix"               DEV.WD  ["subvol=WD/nix" "noatime"  "compress=zstd"])

    (mkfs.btrfs "/opt"               DEV.EVO ["subvol=EVO/opt" "relatime" "compress=zstd"])
    (mkfs.btrfs "/opt/SSE"           DEV.EVO ["subvol=EVO/SSE" "relatime" "compress=zstd"])
    (mkfs.btrfs "/opt/osu"           DEV.EVO ["subvol=EVO/osu" "relatime" "compress=zstd"])
    (mkfs.btrfs "/opt/PrismLauncher" DEV.EVO ["subvol=EVO/PrismLauncher" "relatime" "compress=zstd"])
    (mkfs.btrfs "/srv"               DEV.EVO ["subvol=EVO/srv" "relatime" "compress=zstd"])

    (mkfs.btrfs "/.snapshots/WD"     DEV.WD  ["subvol=WD/.snapshots" "relatime" "compress=zstd"])
    (mkfs.btrfs "/.snapshots/EVO"    DEV.EVO ["subvol=EVO/.snapshots" "relatime" "compress=zstd"])

    {
      swapDevices = [];
      boot.loader.efi.efiSysMountPoint = "/boot/EFI";
      boot.loader.grub.useOSProber = true;
      # dual boot entry so I dont need a seperate bootloader for arch
      boot.loader.grub.extraEntries = ''
        menuentry "arch" {
          insmod btrfs
          search --no-floppy --fs-uuid --set=root ${DEV.WD}
          linux /WD/arch/boot/vmlinuz-linux root=UUID=${DEV.WD} rw rootflags=subvol=WD/arch
          initrd /WD/arch/boot/initramfs-linux.img
        }
      '';
    }
    {
      boot.initrd.availableKernelModules = ["xhci_pci" "ahci" "nvme" "usbhid" "usb_storage" "sd_mod"];
      boot.kernelModules = ["kvm" "kvm_amd"];
      boot.supportedFilesystems = ["btrfs" "ext4" "ntfs"];
      hardware.cpu.amd.updateMicrocode = true;
      hardware.enableRedistributableFirmware = true;
    }
    ({config, ...}: {
      nixpkgs.config.nvidia.acceptLicense = true;
      services.xserver.videoDrivers = ["amdgpu" "nvidia"];
      hardware.nvidia = {
        package = config.boot.kernelPackages.nvidiaPackages.beta;
        modesetting.enable = true;
        powerManagement.enable = false;
        powerManagement.finegrained = false;
        videoAcceleration = true;
        open = false;
      };
      #   environment.variables = {
      #     LIBVA_DRIVER_NAME = "nvidia";
      #     NVD_BACKEND = "direct";
      #     GBM_BACKEND = "nvidia-drm";
      #     __GLX_VENDOR_LIBRARY_NAME = "nvidia";
      #   };
    })
  ];

  home = [
    ({lib, ...}: {
      programs.niri.settings = {
        # must use {} since niri does not like "key = function -float;"
        input.mouse = lib.mkForce {accel-speed = -0.75;};
        input.keyboard.xkb.options = lib.mkForce "ctrl:nocaps,altwin:swap_alt_win";
        outputs."HKC OVERSEAS LIMITED 24E4 0000000000001" = {
          variable-refresh-rate = true;
          mode = {
            width = 1920;
            height = 1080;
            refresh = 165.001;
          };
        };
      };
    })
  ];
}
