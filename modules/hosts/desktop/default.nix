{
  lib,
  config,
  pkgs,
  def,
  ...
}: {
  imports = [
    ./hardware.nix
    ./virt.nix
  ];

  # -----------------------------------------------------------
  # system
  # -----------------------------------------------------------
  boot = {
    initrd.availableKernelModules = ["nvme" "xhci_pci" "ahci" "usb_storage" "usbhid" "sd_mod"]; 
    supportedFilesystems = ["ntfs"];
  };

  system.stateVersion = "24.05"; # DO NOT CHANGE
  networking = {
    interfaces.enp7s0.useDHCP = lib.mkDefault true;
    interfaces.wlp6s0.useDHCP = lib.mkDefault true;
  };

  # -----------------------------------------------------------
  # software
  # -----------------------------------------------------------
  environment.systemPackages = with pkgs; [
    protonup-qt
    protontricks
    prismlauncher
    osu-lazer-bin
  ];

  programs.steam = {
    enable = true;
    remotePlay.openFirewall = true;
    dedicatedServer.openFirewall = true;
    localNetworkGameTransfers.openFirewall = true;
  };
  environment.variables.STEAM_EXTRA_COMPAT_TOOLS_PATHS = "~/.steam/root/compatibilitytools..d";

  services = { 
    xserver.displayManager.sessionCommands = "${lib.getExe pkgs.xrog.xrandr} --output DP-2 --mode 1920x1080 -r 165.00";
  };
}
