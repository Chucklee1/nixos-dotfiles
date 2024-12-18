{
  lib,
  config,
  ...
}: {
  # boot
  boot = {
    initrd.availableKernelModules = ["nvme" "xhci_pci" "usb_storage" "sd_mod"];
    initrd.kernelModules = [];
    kernelModules = ["kvm-amd"];
    extraModulePackages = [];
  };

  # system options
  system.stateVersion = "24.05"; # DO NOT CHANGE
  #networking = {
  #  interfaces.enp7s0.useDHCP = lib.mkDefault true;
  #  interfaces.wlp6s0.useDHCP = lib.mkDefault true;
  #};

  # hardware
  hardware.cpu.amd.updateMicrocode = lib.mkDefault config.hardware.enableRedistributableFirmware;
  services.xserver.videoDrivers = ["radeon"];
}
