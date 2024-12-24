{
  config,
  lib,
  ...
}: {
  imports = [./hardware.nix];
  # boot
  boot = {
    initrd.availableKernelModules = ["nvme" "xhci_pci" "usb_storage" "sd_mod"];
    kernelModules = ["kvm-amd"];
  };

  # system options
  system.stateVersion = "24.05"; # DO NOT CHANGE
  networking.interfaces.wlp2s0.useDHCP = lib.mkDefault true;

  # hardware
  hardware.cpu.amd.updateMicrocode = lib.mkDefault config.hardware.enableRedistributableFirmware;
  services.xserver.videoDrivers = ["radeon"];
}
