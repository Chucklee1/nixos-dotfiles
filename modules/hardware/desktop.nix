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
    supportedFilesystems = ["ntfs"]; 
  };

  fileSystems."/" = {
    device = "/dev/disk/by-uuid/458806b5-8214-4b19-b163-a74016bf29fe";
    fsType = "ext4";
  };

  fileSystems."/boot" = {
    device = "/dev/disk/by-uuid/1C7A-AD30";
    fsType = "vfat";
    options = ["fmask=0077" "dmask=0077"];
  };

  # other drives
  fileSystems."/run/media/goat/BLUE-SATA" = {
    device = "/dev/disk/by-uuid/C814039D14038D9E";
    fsType = "ntfs";
  };
  fileSystems."/run/media/goat/SATA-DRIVE-1" = {
    device = "/dev/disk/by-uuid/2CB23BB6B23B837E   ";
    fsType = "ntfs";
  };
  
  swapDevices = [];

  # Enables DHCP on each ethernet and wireless interface. In case of scripted networking
  # (the default) this is the recommended approach. When using systemd-networkd it's
  # still possible to use this option, but it's recommended to use it in conjunction
  # with explicit per-interface declarations with `networking.interfaces.<interface>.useDHCP`.
  networking.useDHCP = lib.mkDefault true;
  # networking.interfaces.enp7s0.useDHCP = lib.mkDefault true;
  # networking.interfaces.wlp6s0.useDHCP = lib.mkDefault true;

  nixpkgs.hostPlatform = lib.mkDefault "x86_64-linux";
  hardware.cpu.amd.updateMicrocode = lib.mkDefault config.hardware.enableRedistributableFirmware;
}
