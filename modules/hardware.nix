{
  inputs,
  self,
  ...
}: let
  # from sodiboos config
  mkFs = fsType: path: device: options: {
    fileSystems.${path} =
      {inherit device fsType;}
      // (
        if options == null
        then {}
        else {inherit options;}
      );
  };
in {
  nix = {
    desktop = [
      inputs.disko.nixosModules.default
      {hardware.enableRedistributableFirmware = true;}
      (mkFs "ext4" "/media/goat/BLUE_SATA" "/dev/disk/by-uuid/a6ffb4f9-049c-49a1-8b5f-1aca1b8dca08" null)
      (import "${self}/assets/disko/ext4.nix" {device = "/dev/nvme0n1";})
      {
        boot = {
          initrd.availableKernelModules = ["nvme" "xhci_pci" "ahci" "usb_storage" "usbhid" "sd_mod"];
          kernelModules = ["kvm-amd"];
          supportedFilesystems = ["ntfs"];
          loader.grub.gfxmodeEfi = "1920x1080x30,auto";
        };
        hardware.cpu.amd.updateMicrocode = true;
      }
    ];
    laptop = [
      inputs.disko.nixosModules.default
      {hardware.enableRedistributableFirmware = true;}
      (import "${self}/assets/disko/ext4.nix" {device = "/dev/nvme0n1";})
      {
        boot = {
          initrd.availableKernelModules = ["nvme" "xhci_pci" "usb_storage" "sd_mod"];
          kernelModules = ["kvm-amd"];
          loader.grub.gfxmodeEfi = "1920x1080x30,auto";
        };
        hardware.cpu.amd.updateMicrocode = true;
      }
    ];
    umbra = [
      inputs.disko.nixosModules.default
      {hardware.enableRedistributableFirmware = true;}
      (import "${self}/assets/disko/ext4.nix" {device = "/dev/sda";})
      {
        boot.initrd.availableKernelModules = ["nvme" "xhci_pci" "usb_storage" "sd_mod"];
      }
    ];
  };
}
