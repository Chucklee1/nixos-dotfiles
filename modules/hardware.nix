{
  config,
  pkgs,
  def,
  ...
}:
if def.host == "desktop"
then {
  # boot
  boot.supportedFilesystems = ["ntfs"];

  # nvidia
  nixpkgs.config.nvidia.acceptLicense = true;
  services.xserver.videoDrivers = ["nvidia"];
  hardware = {
    graphics = {
      enable = true;
      enable32Bit = true;
      extraPackages = with pkgs; [
        vulkan-tools
        vulkan-loader
        libvdpau-va-gl
        ffmpeg
      ];
    };
    nvidia = {
      modesetting.enable = true;
      package = config.boot.kernelPackages.nvidiaPackages.stable;
      forceFullCompositionPipeline = true;
      videoAcceleration = true;
      nvidiaSettings = true;
      open = false;
    };
  };
  environment.variables = {
    LIBVA_DRIVER_NAME = "nvidia";
    __GLX_VENDOR_LIBRARY_NAME = "nvidia";
    __GL_GSYNC_ALLOWED = "1";
    __GL_VRR_ALLOWED = "1";
    __GL_MaxFramesAllowed = "1";
  };

  # steam
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

  # virtualisation
  programs.virt-manager.enable = true;
  virtualisation = {
    spiceUSBRedirection.enable = true;
    libvirtd = {
      onBoot = "ignore";
      onShutdown = "shutdown";
      enable = true;
      qemu = {
        package = pkgs.qemu_kvm;
        runAsRoot = true;
        swtpm.enable = true;
        ovmf = {
          enable = true;
          packages = [
            (pkgs.OVMF.override {
              secureBoot = true;
              tpmSupport = true;
            })
            .fd
          ];
        };
      };
    };
  };

  home-manager.sharedModules = [
    {
      dconf.settings."org/virt-manager/virt-manager/connections" = {
        autoconnect = ["qemu:///system"];
        uris = ["qemu:///system"];
      };
    }
  ];

  # statusbar
  services.xserver.displayManager.sessionCommands = "xrandr --output DP-2 --mode 1920x1080 --rate 165.00";
  services.dwm-status = {
    order = ["audio" "network" "time"];
    extraConfig = ''
      separator = "    "

      [audio]
      control = "Master"
      mute = "󰝟 X"
      template = "{ICO} {VOL}%"
      icons = ["", "", ""]

      [network]
      no_value = "󰯡"
      template = "󰀂 {LocalIPv4} · {ESSID}"

      [time]
      format = " %Y-%m-%d  %H:%M:%S"
      update_seconds = true
    '';
  };
}
else if def.host == "laptop"
then {
  # hardware
  services.xserver.videoDrivers = ["amdgpu"];
  hardware.amdgpu.amdvlk.enable = true;
  hardware.graphics = {
    enable32Bit = true;
    extraPackages = with pkgs; [
      vulkan-tools
      vulkan-loader
      libvdpau-va-gl
      ffmpeg
    ];
  };

  # statusbar
  services.dwm-status = {
    order = ["audio" "battery" "backlight" "network" "time"];
    extraConfig = ''
      separator = "    "

      [audio]
      control = "Master"
      mute = "󰝟 MUTE"
      template = "{ICO} {VOL}%"
      icons = ["", "", ""]

      [backlight]
      device = "amd_bl1"
      template = "[ICO] {BL}%"
      icons = ["󱩎", "󱩒", "󱩖"]

      [battery]
      charging = ""
      discharging = ""
      enable_notifier = true
      notifier_critical = 10
      notifier_levels = [2, 5, 10, 15, 20]
      separator = " · "
      icons = ["", "", "", "", ""]

      [network]
      no_value = "󰯡"
      template = "󰀂 {LocalIPv4} · {ESSID}"

      [time]
      format = " %Y-%m-%d  %H:%M:%S"
      update_seconds = true
    '';
  };
}
else {}
