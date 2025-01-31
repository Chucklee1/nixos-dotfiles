{
  lib,
  config,
  pkgs,
  ...
}: let
  modules = [
    "gpuGlobal"
    "nvidia"
    "radeon"
    "intelWifi6"
    "weylus"
    "ntfs"
  ];

  mk = import ./libs.nix {inherit lib modules;};
in {
  options = mk.opts;

  config = lib.mkMerge [
    # -----------------------------------------------------------
    # gpus
    # -----------------------------------------------------------
    (lib.mkIf config.gpuGlobal.enable {
      hardware.graphics = {
        enable = true;
        enable32Bit = true;
        extraPackages = with pkgs; [
          vulkan-tools
          vulkan-loader
          libvdpau-va-gl
          ffmpeg
        ];
      };
    })
    (lib.mkIf config.nvidia.enable {
      gpuGlobal.enable = true;
      nixpkgs.config.nvidia.acceptLicense = true;
      services.xserver.videoDrivers = ["nvidia"];
      hardware.nvidia = {
        modesetting.enable = true;
        package = config.boot.kernelPackages.nvidiaPackages.stable;
        forceFullCompositionPipeline = true;
        videoAcceleration = true;
        nvidiaSettings = true;
        open = false;
      };
      environment.variables = {
        LIBVA_DRIVER_NAME = "nvidia";
        __GL_GSYNC_ALLOWED = "1";
        __GL_VRR_ALLOWED = "1";
        __GL_MaxFramesAllowed = "1";
      };
    })
    # wayland nvidia variables
    (lib.mkIf config.nvidia.enable
      (lib.mkIf config.wayland.enable {
        environment.variables = {
          GBM_BACKEND = "nvidia-drm";
          __GLX_VENDOR_LIBRARY_NAME = "nvidia";
        };
      }))
    (lib.mkIf config.radeon.enable {
      gpuGlobal.enable = true;
      services.xserver.videoDrivers = ["amdgpu"];
      hardware.amdgpu.amdvlk.enable = true;
    })

    # -----------------------------------------------------------
    # drivers
    # -----------------------------------------------------------
    (lib.mkIf config.intelWifi6.enable {
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
    })
    (lib.mkIf config.weylus.enable {
      # tablet support
      hardware.uinput.enable = true;
      programs.weylus.enable = true;
      services.udev.extraRules = ''
        KERNEL=="uinput", MODE="0660", GROUP="uinput", OPTIONS+="static_node=uinput"
      '';

      networking.firewall = {
        enable = true;
        allowedTCPPorts = [1701 9001];
        allowedUDPPortRanges = [
          {
            from = 4000;
            to = 4007;
          }
          {
            from = 8000;
            to = 8010;
          }
        ];
      };
    })
    (lib.mkIf config.ntfs.enable {
      boot = {
        supportedFilesystems = ["ntfs"];
        loader.grub.useOSProber = true;
      };
    })
  ];
}
