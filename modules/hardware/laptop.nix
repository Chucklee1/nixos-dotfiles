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
    kernelModules = ["kvm-amd"];
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

    "/boot/efi" = {
      device = "/dev/disk/by-uuid/3092-0E8A";
      fsType = "vfat";
      options = ["fmask=0077" "dmask=0077"];
    };
  };
  swapDevices = [];

  # -----------------------------------------------------------
  # network
  # -----------------------------------------------------------
  services.automatic-timezoned.enable = true; # dynamic timezones
  # timedatectl list-timezones
  # sudo timedatectl set-timezone <timezone> (no "")
  networking.useDHCP = lib.mkDefault true;
  nixpkgs.hostPlatform = lib.mkDefault "x86_64-linux";
  hardware.cpu.amd.updateMicrocode = lib.mkDefault config.hardware.enableRedistributableFirmware;
}
