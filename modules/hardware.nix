{
  inputs,
  self,
  ...
}: {
  desktop.nix = [
    inputs.disko.nixosModules.default
    (import "${self}/assets/disko/desktop.nix")
    {
      boot.initrd.availableKernelModules = ["xhci_pci" "ahci" "nvme" "usbhid" "usb_storage" "sd_mod"];
      boot.kernelModules = ["kvm-amd"];
      boot.supportedFilesystems = ["ext4" "ntfs" "zfs"];
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
      isoImage.showConfiguration = true;
      boot.zfs.forceImportRoot = false;
      stylix.targets.grub.enable = false;
    })
  ];
  installer.graphical.nix = [
    {
      services.displayManager.cosmic-greeter.enable = true;
      services.displayManager.autoLogin.enable = true;
      services.displayManager.autoLogin.user = "nixos";
    }
  ];
}
