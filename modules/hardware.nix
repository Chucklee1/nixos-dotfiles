{
  inputs,
  self,
  ...
}: {
  desktop.nix = [
    ({pkgs, ...}: {
      fileSystems."/" = {
        device = "/dev/disk/by-uuid/ff111615-41d4-4836-8adc-c9374e08bee9";
        fsType = "btrfs";
        options = ["subvol=WD/nixos" "compress=zstd" "relatime"];
      };

      fileSystems."/boot" = {
        device = "/dev/disk/by-uuid/5F4E-A7BB";
        fsType = "vfat";
        options = ["fmask=0022" "dmask=0022"];
      };

      fileSystems."/nix" = {
        device = "/dev/disk/by-uuid/ff111615-41d4-4836-8adc-c9374e08bee9";
        fsType = "btrfs";
        options = ["subvol=WD/nix" "noatime" "compress=zstd"];
      };

      fileSystems."/opt" = {
        device = "/dev/disk/by-uuid/5a5dcb04-31cb-4fae-8fbe-1e8e83a83501";
        fsType = "btrfs";
        options = ["subvol=EVO/opt" "relatime" "compress=zstd"];
      };

      fileSystems."/srv" = {
        device = "/dev/disk/by-uuid/5a5dcb04-31cb-4fae-8fbe-1e8e83a83501";
        fsType = "btrfs";
        options = ["subvol=EVO/srv" "relatime" "compress=zstd"];
      };

      fileSystems."/.snapshots/WD" = {
        device = "/dev/disk/by-uuid/ff111615-41d4-4836-8adc-c9374e08bee9";
        fsType = "btrfs";
        options = ["subvol=WD/.snapshots" "relatime" "compress=zstd"];
      };

      fileSystems."/.snapshots/EVO" = {
        device = "/dev/disk/by-uuid/5a5dcb04-31cb-4fae-8fbe-1e8e83a83501";
        fsType = "btrfs";
        options = ["subvol=EVO/.snapshots" "relatime" "compress=zstd"];
      };

      swapDevices = [];

      boot.initrd.availableKernelModules = ["xhci_pci" "ahci" "nvme" "usbhid" "usb_storage" "sd_mod"];
      boot.kernelModules = ["kvm" "kvm_amd"];
      boot.supportedFilesystems = ["btrfs" "ext4" "ntfs"];
      hardware.cpu.amd.updateMicrocode = true;
      hardware.enableRedistributableFirmware = true;
    })
  ];

  laptop.nix = [
    inputs.disko.nixosModules.default
    (import "${self}/assets/disko/ext4.nix" {device = "/dev/nvme0n1";})
    {
      boot.initrd.availableKernelModules = ["nvme" "xhci_pci" "ahci" "usb_storage" "sd_mod"];
      boot.kernelModules = ["kvm-intel"];
      boot.supportedFilesystems = ["ntfs"];
      hardware.cpu.intel.updateMicrocode = true;
      hardware.enableRedistributableFirmware = true;
    }
  ];

  inspiron.nix = [
    {
      fileSystems."/" = {
        device = "/dev/disk/by-uuid/a06136c2-90a0-445d-89ff-d93ea721f371";
        fsType = "ext4";
      };

      fileSystems."/boot" = {
        device = "/dev/disk/by-uuid/08F1-E578";
        fsType = "vfat";
        options = ["fmask=0022" "dmask=0022"];
      };

      swapDevices = [];

      boot.initrd.availableKernelModules = ["xhci_pci" "ahci" "usb_storage" "sd_mod" "sr_mod" "rtsx_usb_sdmmc"];
      boot.kernelModules = ["kvm-intel"];
      hardware.cpu.intel.updateMicrocode = true;
      hardware.enableRedistributableFirmware = true;
    }
  ];

  umbra.nix = [
    ({modulesPath, ...}: {
      imports = ["${modulesPath}/installer/cd-dvd/installation-cd-minimal.nix"];
      boot.zfs.forceImportRoot = false;
      isoImage = {
        showConfiguration = true;
        configurationName = "niri wayland (zsh + zfs)";
      };
      stylix.targets.grub.enable = false;
      services.displayManager = {
        cosmic-greeter.enable = true;
        autoLogin.enable = true;
        autoLogin.user = "nixos";
      };
    })
  ];
}
