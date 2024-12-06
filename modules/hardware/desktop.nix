{
  config,
  lib,
  pkgs,
  modulesPath,
  ...
}: {
  imports = [
    (modulesPath + "/installer/scan/not-detected.nix")
  ];

  boot.initrd.availableKernelModules = ["nvme" "xhci_pci" "ahci" "usb_storage" "usbhid" "sd_mod"];
  boot.initrd.kernelModules = [];
  boot.kernelModules = ["kvm-amd"];
  boot.extraModulePackages = [];

  fileSystems = {
    "/" = {
      device = "/dev/disk/by-uuid/639a16aa-ad25-47e1-98b4-825dd563c53d";
      fsType = "ext4";
    };

    "/boot/efi" = {
      device = "/dev/disk/by-uuid/0587-E8FD";
      fsType = "vfat";
      options = ["fmask=0022" "dmask=0022"];
    };

    # other drives
    "/home/goat/games" = {
      device = "/dev/disk/by-uuid/C814039D14038D9E";
      fsType = "ntfs";
    };
    "/run/media/goat/SATA-DRIVE-1" = {
      device = "/dev/disk/by-uuid/2CB23BB6B23B837E   ";
      fsType = "ntfs";
    };
  };

  swapDevices = [];

  networking.useDHCP = lib.mkDefault true;
  networking.interfaces.wlp6s0.useDHCP = lib.mkDefault true;
  nixpkgs.hostPlatform = lib.mkDefault "x86_64-linux";
  hardware.cpu.amd.updateMicrocode = lib.mkDefault config.hardware.enableRedistributableFirmware;
}
