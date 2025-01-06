{
  lib,
  config,
  pkgs,
  def,
  ...
}: {
  imports = [./hardware.nix];

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

  # -----------------------------------------------------------
  # hardware
  # -----------------------------------------------------------
  hardware = {
    cpu.amd.updateMicrocode = lib.mkDefault config.hardware.enableRedistributableFirmware;
    graphics.extraPackages = with pkgs; [
      nvidia-vaapi-driver
      vaapiVdpau
      libvdpau-va-gl
    ];
    nvidia = {
      modesetting.enable = true;
      package = config.boot.kernelPackages.nvidiaPackages.stable;
      videoAcceleration = true;
      powerManagement.enable = false;
      powerManagement.finegrained = false;
      open = false;
    };
  };
  nixpkgs.config.nvidia.acceptLicense = true;
  services.xserver.videoDrivers = ["nvidia"];

  # -----------------------------------------------------------
  # dwm
  # -----------------------------------------------------------
  services.xserver = {
    enable = true;
    xkb.layout = def.layout;
    # dwm override
    windowManager.dwm = {
      enable = true;
      package = pkgs.dwm.overrideAttrs (old: {src = def.dwm-src;});
    };
    # startup command
    displayManager.sessionCommands = let
      feh = lib.getExe pkgs.feh;
      xrandr = lib.getExe pkgs.xorg.xrandr;
    in ''
      if [ "$XDG_SESSION_DESKTOP" = "none+dwm" ]; then
        ${feh} --bg-scale /path/to/wallpaper.jpg &
        ${xrandr} xrandr --output DP-2 --mode 1920x1080 -r 165.00
      fi
    '';
  };

  # -----------------------------------------------------------
  # software
  # -----------------------------------------------------------
  environment.systemPackages = with pkgs; [
    protonup-qt
    protontricks
    prismlauncher
    rofi
  ];

  programs.steam = {
    enable = true;
    remotePlay.openFirewall = true;
    dedicatedServer.openFirewall = true;
    localNetworkGameTransfers.openFirewall = true;
  };

  # -----------------------------------------------------------
  # global variables
  # -----------------------------------------------------------
  environment.variables = {
    STEAM_EXTRA_COMPAT_TOOLS_PATHS = "~/.steam/root/compatibilitytools..d";
    MOZ_DISABLE_RDD_SANDBOX = "1";
    _JAVA_AWT_WM_NONREPARENTING = "1";
    LIBVA_DRIVER_NAME = "nvidia";
    __GLX_VENDOR_LIBRARY_NAME = "nvidia";
    __GL_GSYNC_ALLOWED = "1";
    __GL_VRR_ALLOWED = "1";
    __GL_MaxFramesAllowed = "1";
  };

  # -----------------------------------------------------------
  # extra niri settings
  # -----------------------------------------------------------
  home-manager.sharedModules = [
    {
      programs.niri.settings = {
        variables = {
          GBM_BACKEND = "nvidia-drm";
          NVD_BACKEND = "direct";
          WLR_NO_HARDWARE_CURSORS = "1";
          WLR_RENDERER_ALLOW_SOFTWARE = "1";
          ELECTRON_OZONE_PLATFORM_HINT = "auto";
        };
        # monitors
        outputs."DP-2" = {
          enable = true;
          mode = {
            width = 1920;
            height = 1080;
            refresh = 165.001;
          };
          position.x = 0;
          position.y = 0;
        };
      };
    }
  ];
}
