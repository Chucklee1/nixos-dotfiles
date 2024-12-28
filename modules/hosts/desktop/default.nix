{
  lib,
  config,
  pkgs,
  def,
  ...
}: {
  imports = [
    ./hardware.nix
    ./nvidia.nix
    ./hyprland.nix
    ./virt.nix
  ];

  # -----------------------------------------------------------
  # system
  # -----------------------------------------------------------
  boot = {
    initrd.availableKernelModules = ["nvme" "xhci_pci" "ahci" "usb_storage" "usbhid" "sd_mod"];
    kernelModules = ["kvm-amd"];
    supportedFilesystems = ["ntfs"];
  };

  system.stateVersion = "24.05"; # DO NOT CHANGE
  networking = {
    interfaces.enp7s0.useDHCP = lib.mkDefault true;
    interfaces.wlp6s0.useDHCP = lib.mkDefault true;
  };

  hardware.cpu.amd.updateMicrocode = lib.mkDefault config.hardware.enableRedistributableFirmware;

  # -----------------------------------------------------------
  # packages
  # -----------------------------------------------------------
  environment.systemPackages = with pkgs; [
    # tools/deps
    zenity
    wineWowPackages.stagingFull
    samba
    winetricks
    protonup-qt
    protontricks
    # apps/games
    webcord
    osu-lazer-bin
    prismlauncher
  ];

  programs = {
    gamemode.enable = true;
    steam = {
      enable = true;
      remotePlay.openFirewall = true;
      dedicatedServer.openFirewall = true;
      localNetworkGameTransfers.openFirewall = true;
    };
  };

  environment.variables = {STEAM_EXTRA_COMPAT_TOOLS_PATHS = "~/.steam/root/compatibilitytools.d";};
}
