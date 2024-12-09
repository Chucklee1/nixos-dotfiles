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
  disko.devices.disk.main = {
    device = "/dev/disk/by-id/some-disk-id";
    type = "disk";
    content = {
      type = "gpt";
      partitions = {
        MBR = {
          type = "EF02"; # for grub MBR
          size = "1M";
          priority = 1; # Needs to be first partition
        };
        ESP = {
          type = "EF00";
          size = "500M";
          content = {
            type = "filesystem";
            format = "vfat";
            mountpoint = "/boot";
            mountOptions = [ "umask=0077" ];
          };
        };
        root = {
          size = "100%";
          content = {
            type = "filesystem";
            format = "ext4";
            mountpoint = "/";
          };
        };
      };
    };
  };

  # -----------------------------------------------------------
  # network
  # -----------------------------------------------------------
  time.timeZone = "America/Vancouver"; # static timezone
  networking.useDHCP = lib.mkDefault true;
  networking.interfaces.wlp6s0.useDHCP = lib.mkDefault true;
  nixpkgs.hostPlatform = lib.mkDefault "x86_64-linux";
  hardware.cpu.amd.updateMicrocode = lib.mkDefault config.hardware.enableRedistributableFirmware;
}