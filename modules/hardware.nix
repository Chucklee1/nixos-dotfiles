{
  inputs,
  self,
  ...
}: {
  desktop.nix = [
    inputs.disko.nixosModules.default
    (import "${self}/assets/disko/ext-home.nix" {
      main = "/dev/nvme0n1";
      home = "/dev/sda";
    })
    {
      boot.initrd.availableKernelModules = ["nvme" "xhci_pci" "ahci" "usb_storage" "usbhid" "sd_mod"];
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
