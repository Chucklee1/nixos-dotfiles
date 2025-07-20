{
  inputs,
  self,
  ...
}: let
  cpu = {
    amd = {lib, ...}: {
      boot.kernelModules = lib.mkAfter ["kvm-amd"];
      hardware.cpu.amd.updateMicrocode = lib.mkDefault true;
    };
    intel = {lib, ...}: {
      hardware.cpu.amd.updateMicrocode = lib.mkDefault true;
    };
  };
in {
  desktop.nix = [
    inputs.disko.nixosModules.default
    (import "${self}/assets/disko/ext-home.nix" {
      main = "/dev/nvme0n1";
      home = "/dev/sda";
    })
    cpu.amd
    {
      boot.initrd.availableKernelModules = ["nvme" "xhci_pci" "ahci" "usb_storage" "usbhid" "sd_mod"];
      boot.supportedFilesystems = ["ntfs"];
      hardware.enableRedistributableFirmware = true;
    }
  ];

  laptop.nix = [
    inputs.disko.nixosModules.default
    (import "${self}/assets/disko/ext-home.nix" {device = "/dev/nvme0n1";})
    cpu.intel
    {
      boot.supportedFilesystems = ["ntfs"];
      hardware.enableRedistributableFirmware = true;
    }
  ];
}
