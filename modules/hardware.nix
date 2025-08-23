{
  inputs,
  self,
  ...
}: {
  desktop.nix = [
    {
      fileSystems."/" = { 
          device = "/dev/disk/by-label/WD_ROOT";
          fsType = "btrfs";
          options = [ "subvol=@nixos" ];
        };

      fileSystems."/nix" = {
          device = "/dev/disk/by-label/WD_ROOT";
          fsType = "btrfs";
          options = [ "subvol=@nix" ];
        };

      fileSystems."/srv" = {
          device = "/dev/disk/by-label/BLUE_SATA";
          fsType = "btrfs";
          options = [ "subvol=@srv" ];
        };

      fileSystems."/opt" = {
          device = "/dev/disk/by-label/BLUE_SATA";
          fsType = "btrfs";
          options = [ "subvol=@opt" ];
        };

      fileSystems."/boot" = {
          device = "/dev/disk/by-label/WD_BOOT";
          fsType = "vfat";
          options = [ "fmask=0022" "dmask=0022" ];
        };

      swapDevices = [ ];

      boot.initrd.availableKernelModules = ["xhci_pci" "ahci" "nvme" "usbhid" "usb_storage" "sd_mod"];
      boot.kernelModules = ["kvm-amd"];
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
