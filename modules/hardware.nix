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
  nix.desktop = [
    inputs.disko.nixosModules.default
    (import "${self}/assets/disko/ext-home.nix" {
      main = "/dev/nvme0n1";
      home = "/dev/sda";
    })
    cpu.amd
    {
      boot.initrd.availableKernelModules = ["nvme" "xhci_pci" "ahci" "usb_storage" "usbhid" "sd_mod"];
      boot.supportedFilesystems = ["ntfs"];
      boot.loader.grub.gfxmodeEfi = "1920x1080x30,auto";
      hardware.enableRedistributableFirmware = true;
    }
  ];
}
