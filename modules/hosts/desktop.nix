{
  nix = [
    (let
      WD_UUID = "ff111615-41d4-4836-8adc-c9374e08bee9";
      EVO_UUID = "5a5dcb04-31cb-4fae-8fbe-1e8e83a83501";
    in {
      fileSystems."/" = {
        device = "/dev/disk/by-uuid/${WD_UUID}";
        fsType = "btrfs";
        options = ["subvol=WD/nixos" "relatime" "compress=zstd"];
      };

      fileSystems."/boot" = {
        device = "/dev/disk/by-uuid/${WD_UUID}";
        fsType = "btrfs";
        options = ["subvol=WD/nixos/boot" "noatime"];
      };

      fileSystems."/boot/EFI" = {
        device = "/dev/disk/by-uuid/5F4E-A7BB";
        fsType = "vfat";
        options = ["fmask=0022" "dmask=0022"];
      };

      fileSystems."/nix" = {
        device = "/dev/disk/by-uuid/${WD_UUID}";
        fsType = "btrfs";
        options = ["subvol=WD/nix" "noatime" "compress=zstd"];
      };

      fileSystems."/opt" = {
        device = "/dev/disk/by-uuid/${EVO_UUID}";
        fsType = "btrfs";
        options = ["subvol=EVO/opt" "relatime" "compress=zstd"];
      };

      fileSystems."/home/goat/SSE" = {
        device = "/dev/disk/by-uuid/${EVO_UUID}";
        fsType = "btrfs";
        options = ["subvol=EVO/SSE" "relatime" "compress=zstd"];
      };

      fileSystems."/home/goat/.local/share/osu" = {
        device = "/dev/disk/by-uuid/${EVO_UUID}";
        fsType = "btrfs";
        options = ["subvol=EVO/osu" "relatime" "compress=zstd"];
      };

      fileSystems."/home/goat/.local/share/PrismLauncher" = {
        device = "/dev/disk/by-uuid/${EVO_UUID}";
        fsType = "btrfs";
        options = ["subvol=EVO/PrismLauncher" "relatime" "compress=zstd"];
      };

      fileSystems."/srv" = {
        device = "/dev/disk/by-uuid/${EVO_UUID}";
        fsType = "btrfs";
        options = ["subvol=EVO/srv" "relatime" "compress=zstd"];
      };

      fileSystems."/.snapshots/WD" = {
        device = "/dev/disk/by-uuid/${WD_UUID}";
        fsType = "btrfs";
        options = ["subvol=WD/.snapshots" "relatime" "compress=zstd"];
      };

      fileSystems."/.snapshots/EVO" = {
        device = "/dev/disk/by-uuid/${EVO_UUID}";
        fsType = "btrfs";
        options = ["subvol=EVO/.snapshots" "relatime" "compress=zstd"];
      };

      swapDevices = [];

      boot.loader.efi.efiSysMountPoint = "/boot/EFI";
      boot.loader.grub.useOSProber = true;
      # dual boot entry so I dont need a seperate bootloader for arch
      boot.loader.grub.extraEntries = ''
        menuentry "arch" {
          insmod btrfs
          search --no-floppy --fs-uuid --set=root ${WD_UUID}
          linux /WD/arch/boot/vmlinuz-linux root=UUID=${WD_UUID} rw rootflags=subvol=WD/arch
          initrd /WD/arch/boot/initramfs-linux.img
        }
      '';
    })
    {
      boot.initrd.availableKernelModules = ["xhci_pci" "ahci" "nvme" "usbhid" "usb_storage" "sd_mod"];
      boot.kernelModules = ["kvm" "kvm_amd"];
      boot.supportedFilesystems = ["btrfs" "ext4" "ntfs"];
      hardware.cpu.amd.updateMicrocode = true;
      hardware.enableRedistributableFirmware = true;
    }
  ];

  home = [
    ({lib, ...}: {
      programs.niri.settings = {
        # must use {} since niri does not like "key = function -float;"
        input.mouse = lib.mkForce {accel-speed = -0.75;};
        input.keyboard.xkb.options = lib.mkForce "ctrl:nocaps,altwin:swap_alt_win";
        outputs."DP-1".mode = {
          width = 1920;
          height = 1080;
          refresh = 165.001;
        };
      };
    })
  ];
}
