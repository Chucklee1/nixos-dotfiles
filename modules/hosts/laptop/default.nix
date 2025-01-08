{
  lib,
  config,
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
  # hardware
  #services.xserver.videoDrivers = ["radeon"];
}
