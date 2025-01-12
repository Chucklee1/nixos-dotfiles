{
  lib,
  config,
  ...
}: {
  imports = [./hardware.nix];

  # the rest
  boot.initrd.availableKernelModules = ["nvme" "xhci_pci" "usb_storage" "sd_mod"];
  system.stateVersion = "24.05"; # DO NOT CHANGE
}
