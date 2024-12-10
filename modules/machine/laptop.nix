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
    initrd.availableKernelModules = ["nvme" "xhci_pci" "usb_storage" "sd_mod"];
    initrd.kernelModules = [];
    kernelModules = [];
    extraModulePackages = [];
  };

  # -----------------------------------------------------------
  # partitions
  # -----------------------------------------------------------
  fileSystems = {
    "/" = {
      device = "/dev/disk/by-uuid/4b8f2d77-499c-49f2-b85e-726178d1e6e2";
      fsType = "ext4";
    };

    "/boot" = {
      device = "/dev/disk/by-label/BOOT";
      fsType = "vfat";
      options = ["fmask=0077" "dmask=0077"];
    };
  };
  swapDevices = [];
}
