{
  inputs,
  self,
  ...
}: {
  desktop.nix = [
    {
      fileSystems."/" = {
        device = "/dev/disk/by-uuid/87f9df33-72a9-4841-9326-5200086e5f03";
        fsType = "btrfs";
        options = [ "subvol=@nixos" "compress=zstd" "relatime" ];
      };

      fileSystems."/nix" = {
        device = "/dev/disk/by-uuid/87f9df33-72a9-4841-9326-5200086e5f03";
        fsType = "btrfs";
        options = [ "subvol=@nix" "compress=zstd" "noatime" ];
      };

      fileSystems."/boot" = {
        device = "/dev/disk/by-uuid/AA80-5227";
        fsType = "vfat";
        options = [ "fmask=0022" "dmask=0022" ];
      };

      fileSystems."/opt" = {
        device = "/dev/disk/by-uuid/ed8922cf-d9fc-41c5-aae4-3449e71626cc";
        fsType = "btrfs";
        options = [ "subvol=opt" "compress=zstd" "relatime" ];
      };

      fileSystems."/srv" = {
        device = "/dev/disk/by-uuid/ed8922cf-d9fc-41c5-aae4-3449e71626cc";
        fsType = "btrfs";
        options = [ "subvol=srv" "compress=zstd" "relatime" ];
      };

      swapDevices = [ ];

      boot.initrd.availableKernelModules = ["xhci_pci" "ahci" "nvme" "usbhid" "usb_storage" "sd_mod"];
      boot.kernelParams = ["amd_iommu=on" "iommu=pt"];
      boot.kernelModules = ["kvm" "kvm_amd"];
      boot.supportedFilesystems = ["btrfs" "ext4" "ntfs" "zfs"];
      networking.hostId = "8425e349";
      hardware.cpu.amd.updateMicrocode = true;
      hardware.enableRedistributableFirmware = true;
    }
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
