{
  lib,
  config,
  ...
}: {
  imports = [./hardware.nix];
  
  # toggle module options
  amdcpu.enable = true;
  amdgpu.enable = true;
  nvidia.enable = false;

  # the rest
  boot.initrd.availableKernelModules = ["nvme" "xhci_pci" "usb_storage" "sd_mod"];
  system.stateVersion = "24.05"; # DO NOT CHANGE
  services.dwm-status.order = ["audio" "backlight" "battery" "network" "time"]; 
  
}
