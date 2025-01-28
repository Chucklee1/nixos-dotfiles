{
  config,
  lib,
  pkgs,
  def,
  ...
}:
if def.host == "desktop"
then {
  # other drives
  fileSystems."/media/goat/BLUE_SATA" = {
    device = "/dev/disk/by-uuid/a6ffb4f9-049c-49a1-8b5f-1aca1b8dca08";
    fsType = "ext4";
  };

  # windows...
  boot = {
    supportedFilesystems = ["ntfs"];
    loader.grub.useOSProber = true;
  };

  networking.interfaces.enp7s0.useDHCP = lib.mkDefault true;
  networking.interfaces.wlp6s0.useDHCP = lib.mkDefault true;

  /*
    boot.kernelModules = [
    "iwlwifi"
    "iwlmvm"
  ];
  boot.kernelParams = [
    "iwlwifi.11n-disable=1"
    "iwlwifi.swcrypto=0"
    "iwlwifi.bt_coex_active=0"
    "iwlwifi.power_save=0"
    "iwlmvm.power_scheme=0"
    "iwlwifi.d0i3_disable=1"
    "iwlwifi.uapsd_disable=1"
    "iwlwifi.lar_disable=1"
  ];
  */

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
    __GL_GSYNC_ALLOWED = "1";
    __GL_VRR_ALLOWED = "1";
    __GL_MaxFramesAllowed = "1";
    # needed for wayland
    GBM_BACKEND = "nvidia-drm";
    __GLX_VENDOR_LIBRARY_NAME = "nvidia";
  };

  # steam
  environment.systemPackages = with pkgs; [
    prismlauncher
    osu-lazer-bin
  ];

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
}
else {}
