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

  boot = {
    initrd.availableKernelModules = ["nvme" "xhci_pci" "ahci" "usb_storage" "usbhid" "sd_mod"];
    initrd.kernelModules = [];
    kernelModules = ["kvm-amd"];
    extraModulePackages = [];
  };

  fileSystems."/" = {
    device = "LABEL=NIXOS-ROOT";
    fsType = "ext4";
  };

  fileSystems."/boot" = {
    device = "label=BOOT";
    fsType = "vfat";
    options = ["fmask=0022" "dmask=0022"];
  };

  fileSystems."/media/goat/GAMES" = {
    device = "/dev/disk/by-uuid/56edc1af-6e52-4b45-8fc0-8ddad9e4cbf6";
    fsType = "ext4";
  };

  swapDevices = [];

  networking.useDHCP = lib.mkDefault true;
  networking.interfaces.enp7s0.useDHCP = lib.mkDefault true;
  networking.interfaces.wlp6s0.useDHCP = lib.mkDefault true;
}
