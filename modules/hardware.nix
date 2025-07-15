{
  inputs,
  self,
  ...
}:
{
  nix.desktop = [
    inputs.disko.nixosModules.default
    (import "${self}/assets/disko/ext-home.nix" {
      main = "/dev/nvme0n1";
      home = "/dev/sda";
    })
    {
      boot = {
        initrd.availableKernelModules = [
          "nvme"
          "xhci_pci"
          "ahci"
          "usb_storage"
          "usbhid"
          "sd_mod"
        ];
        kernelModules = [ "kvm-amd" ];
        supportedFilesystems = [ "ntfs" ];
        loader.grub.gfxmodeEfi = "1920x1080x30,auto";
      };
      hardware.enableRedistributableFirmware = true;
      hardware.cpu.amd.updateMicrocode = true;
    }
  ];
}
