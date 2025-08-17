{
  linux.nix = [
    ({pkgs, ...}: {
      # gpu
      hardware.graphics = {
        enable = true;
        enable32Bit = true;
        extraPackages = with pkgs; [
          vulkan-tools
          vulkan-loader
          libvdpau-va-gl
        ];
      };

      # audio
      security.rtkit.enable = true;
      services.pipewire = {
        enable = true;
        alsa.enable = true;
        alsa.support32Bit = true;
        pulse.enable = true;
      };

      # bluetooth
      hardware.bluetooth.enable = true;
      services.blueman.enable = true;

      # ssh
      services.openssh = {
        enable = true;
        settings = {
          PasswordAuthentication = false;
          PermitRootLogin = "prohibit-password";
        };
      };

      # misc
      services = {
        printing.enable = true;
        gvfs.enable = true;
      };
    })
  ];

  metal.nix = [{services.fstrim.enable = true;}];

  drivers = {
    nvidia.nix = [
      ({config, ...}: {
        nixpkgs.config.nvidia.acceptLicense = true;
        services.xserver.videoDrivers = ["nvidia"];
        hardware.nvidia = {
          modesetting.enable = true;
          package = config.boot.kernelPackages.nvidiaPackages.beta;
          videoAcceleration = true;
          open = false;
        };
        environment.variables = {
          LIBVA_DRIVER_NAME = "nvidia";
          NVD_BACKEND = "direct";
          GBM_BACKEND = "nvidia-drm";
          __GLX_VENDOR_LIBRARY_NAME = "nvidia";
        };
      })
    ];
    tablet.nix = [
      {
        hardware.uinput.enable = true;
        programs.weylus.enable = true;
        services.udev.extraRules = ''KERNEL=="uinput", MODE="0660", GROUP="uinput", OPTIONS+="static_node=uinput" '';
      }
    ];
  };
}
