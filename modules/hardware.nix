{
  self,
  def,
  ...
}: {
  global = [
    "${self}/modules/${def.host}.gen.nix"
    ({pkgs, ...}: {
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
  ];

  desktop = [
    # general hardware
    ({lib, ...}: {
      fileSystems."/media/goat/BLUE_SATA" = {
        device = "/dev/disk/by-uuid/a6ffb4f9-049c-49a1-8b5f-1aca1b8dca08";
        fsType = "ext4";
      };
      boot = {
        supportedFilesystems = ["ntfs"];
        loader.grub.useOSProber = true;
      };
      networking.interfaces.enp7s0.useDHCP = lib.mkDefault true;
      networking.interfaces.wlp6s0.useDHCP = lib.mkDefault true;
    })
    # nvidia
    ({config, ...}: {
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
        # wayland
        GBM_BACKEND = "nvidia-drm";
        __GLX_VENDOR_LIBRARY_NAME = "nvidia";
      };
    })
    {
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
    }
  ];

  laptop = [
    {
      services.xserver.videoDrivers = ["amdgpu"];
      hardware.amdgpu.amdvlk.enable = true;
    }
  ];
}
