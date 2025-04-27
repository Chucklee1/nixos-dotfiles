{
  inputs,
  self,
  ...
}: {
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
    (import "${self}/assets/disko/ext4.nix" {device = "/dev/nvme0n1";})
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
  nix.macbook = [
    inputs.disko.nixosModules.default
    (import "${self}/assets/disko/btrfs.nix" {device = "/dev/sda";})
    {
      boot = {
        # TODO
        supportedFilesystems = ["ntfs" "btrfs" "apfs"];
      };
      hardware.enableRedistributableFirmware = true;
    }
  ];
}
