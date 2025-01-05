{
  lib,
  config,
  pkgs,
  def,
  ...
}: {
  imports = [
    ./hardware.nix 
  ];
  home-manager.sharedModules = [./dwm.nix];
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

  hardware = {
    cpu.amd.updateMicrocode = lib.mkDefault config.hardware.enableRedistributableFirmware;
    nvidia = {
      modesetting.enable = true;
      package = config.boot.kernelPackages.nvidiaPackages.stable;
      videoAcceleration = true;
      powerManagement.enable = false;
      powerManagement.finegrained = false;
      open = false;
    };
    graphics.extraPackages = with pkgs; [
      nvidia-vaapi-driver
      vaapiVdpau
      libvdpau-va-gl
    ];
  };
  nixpkgs.config.nvidia.acceptLicense = true;
  services.xserver.videoDrivers = ["nvidia"];

  # -----------------------------------------------------------
  # packages
  # -----------------------------------------------------------
  environment.systemPackages = with pkgs; [
    # tools/deps
    protonup-qt
    protontricks
    # apps/games
    prismlauncher
  ];

  programs = {
    steam = {
      enable = true;
      remotePlay.openFirewall = true;
      dedicatedServer.openFirewall = true;
      localNetworkGameTransfers.openFirewall = true;
    };
  };
  environment.variables = {
    STEAM_EXTRA_COMPAT_TOOLS_PATHS = "~/.steam/root/compatibilitytools.d";
    GBM_BACKEND = "nvidia-drm";
    LIBVA_DRIVER_NAME = "nvidia";
    __GLX_VENDOR_LIBRARY_NAME = "nvidia";
    NVD_BACKEND = "direct";
    __GL_GSYNC_ALLOWED = "1";
    __GL_VRR_ALLOWED = "1";
    __GL_MaxFramesAllowed = "1";
    WLR_NO_HARDWARE_CURSORS = "1";
    WLR_RENDERER_ALLOW_SOFTWARE = "1";
    ELECTRON_OZONE_PLATFORM_HINT = "auto";
    MOZ_DISABLE_RDD_SANDBOX = "1";
    _JAVA_AWT_WM_NONREPARENTING = "1";
  };
}
