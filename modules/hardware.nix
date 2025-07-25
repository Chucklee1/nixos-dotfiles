{
  inputs,
  self,
  ...
}: {
  desktop.nix = [
    inputs.disko.nixosModules.default
    (import "${self}/assets/disko/desktop.nix")
    {
      fileSystems."/home" =
        { device = "/dev/disk/by-uuid/5537e616-686a-483e-b4e4-3a7a956ba348";
          fsType = "ext4";
        };

      swapDevices = [ ];

			boot.initrd.availableKernelModules = ["xhci_pci" "ahci" "nvme" "usbhid" "usb_storage" "sd_mod"];
      boot.kernelModules = ["kvm-amd"];
      boot.supportedFilesystems = ["ntfs"];
      hardware.cpu.amd.updateMicrocode = true;
      hardware.enableRedistributableFirmware = true;
      boot.loader.grub.useOSProber = true;
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
}
