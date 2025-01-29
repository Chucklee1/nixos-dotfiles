{
  lib,
  config,
  pkgs,
  def,
  ...
}: {
  options = with def.mk.opt; {
    nvidia.enable = opt;
    radeon.enable = opt;
    intelWifi6.enable = opt;
    weylus.enable = opt;
    windowsCompat = opt;
  };
  config = with def.mk.conf;
    lib.mkMerge [
      # -----------------------------------------------------------
      # gpus
      # -----------------------------------------------------------
      (conf "gpuGlobal" {})
      (conf "nvidia" {
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
        environment.variables =
          {
            LIBVA_DRIVER_NAME = "nvidia";
            __GL_GSYNC_ALLOWED = "1";
            __GL_VRR_ALLOWED = "1";
            __GL_MaxFramesAllowed = "1";
          }
          ++ conf "wayland" {
            # needed for wayland
            GBM_BACKEND = "nvidia-drm";
            __GLX_VENDOR_LIBRARY_NAME = "nvidia";
          };
      })
      (conf "radeon" {
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
      })
      # -----------------------------------------------------------
      # drivers
      # -----------------------------------------------------------
      (conf "intelWifi6" {
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
      (conf "weylus" {
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
      (conf "windowsCompat" {
        boot = {
          supportedFilesystems = ["ntfs"];
          loader.grub.useOSProber = true;
        };
      })
    ];
}
