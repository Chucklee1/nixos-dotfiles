{inputs, ...}:
/*
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
in
*/
{
  nix.global = [
    ({modulesPath, ...}: {
      imports = [(modulesPath + "/installer/scan/not-detected.nix")];
    })
  ];

  nix.desktop = [
    {
      fileSystems."/" = {
        device = "/dev/disk/by-uuid/96c41aaf-846f-47b1-8319-eed5a3a32294";
        fsType = "ext4";
      };

      fileSystems."/boot" = {
        device = "/dev/disk/by-uuid/75D4-A9F7";
        fsType = "vfat";
        options = ["fmask=0022" "dmask=0022"];
      };

      fileSystems."/media/goat/BLUE_SATA" = {
        device = "/dev/disk/by-uuid/a6ffb4f9-049c-49a1-8b5f-1aca1b8dca08";
        fsType = "ext4";
      };

      swapDevices = [];
      boot = {
        initrd.availableKernelModules = ["nvme" "xhci_pci" "ahci" "usb_storage" "usbhid" "sd_mod"];
        initrd.kernelModules = [];
        extraModulePackages = [];
        kernelModules = ["kvm-amd"];
        supportedFilesystems = ["ntfs"];
      };
      hardware.cpu.amd.updateMicrocode = true;
    }
  ];
  nix.laptop = [
    inputs.disko.nixosModules.default
    (import ../assets/ext4.nix {device = "/dev/nvme0n1";})
    {
      boot = {
        initrd.availableKernelModules = ["nvme" "xhci_pci"];
        initrd.kernelModules = [];
        kernelModules = ["kvm-amd"];
        extraModulePackages = [];
      };
      hardware.cpu.amd.updateMicrocode = true;
    }
  ];
  nix.nimbus = [
    inputs.disko.nixosModules.default
    (import ../assets/btrfs.nix {device = "/dev/sdb";})
    {
      boot = {
        initrd.availableKernelModules = ["nvme" "xhci_pci" "uas" "usb_storage" "sd_mod"];
        initrd.kernelModules = ["dm-snapshot"];
        # kernelModules = [ ];
        # extraModulePackages = [ ];
        supportedFilesystems = ["ntfs" "btrfs" "apfs"];
      };
      hardware.enableRedistributableFirmware = true;
    }
  ];
}
