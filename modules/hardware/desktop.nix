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

  # -----------------------------------------------------------
  # kernel
  # -----------------------------------------------------------
  boot = {
    initrd.availableKernelModules = ["nvme" "xhci_pci" "ahci" "usb_storage" "usbhid" "sd_mod"];
    initrd.kernelModules = [];
    kernelModules = ["kvm-amd"];
    extraModulePackages = [];
    supportedFilesystems = ["ntfs"]; # ntfs drives
  };

  # -----------------------------------------------------------
  # partitions
  # -----------------------------------------------------------
  fileSystems = {
    "/" = {
      device = "/dev/disk/by-uuid/639a16aa-ad25-47e1-98b4-825dd563c53d";
      fsType = "ext4";
    };

    "/boot" = {
      device = "/dev/disk/by-uuid/0587-E8FD";
      fsType = "vfat";
      options = ["fmask=0022" "dmask=0022"];
    };
  };

  swapDevices = [];

  # -----------------------------------------------------------
  # network
  # -----------------------------------------------------------
  networking.useDHCP = lib.mkDefault true;
  networking.interfaces.wlp6s0.useDHCP = lib.mkDefault true;
  nixpkgs.hostPlatform = lib.mkDefault "x86_64-linux";
  hardware.cpu.amd.updateMicrocode = lib.mkDefault config.hardware.enableRedistributableFirmware;
}
