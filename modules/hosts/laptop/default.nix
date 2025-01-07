{
  lib,
  config,
  pkgs,
  ...
}: {
  imports = [./hardware.nix];

  # extra system options
  boot.initrd.availableKernelModules = ["nvme" "xhci_pci" "usb_storage" "sd_mod"];
  boot.kernelModules = ["kvm-amd"];
  system.stateVersion = "24.05"; # DO NOT CHANGE
  networking.interfaces.wlp2s0.useDHCP = lib.mkDefault true;
  hardware.cpu.amd.updateMicrocode = lib.mkDefault config.hardware.enableRedistributableFirmware;

  # xserver
  services.xserver = {
    videoDrivers = ["radeon"];
    windowManager.dwm = {
      enable = true;
      package = /home/goat/dwm;
    };
  };

  environment.systemPackages = with pkgs; [
    st
    dmenu
  ];
}
