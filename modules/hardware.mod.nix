{
  nix-laptop = [
    {
      config,
      lib,
      pkgs,
      ...
    }: {
      imports = [./laptop.autogen.nix];
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
  ];

  nix-desktop = [
    {
      config,
      lib,
      pkgs,
      ...
    }: {
      imports = [./desktop.autogen.nix];
      # boot
      boot.supportedFilesystems = is.its.desktop ["ntfs"];

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
    }
  ];
}
