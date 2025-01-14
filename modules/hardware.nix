{
  nixMod.laptop = [
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
      services.dwm-status.order = ["audio" "battery" "backlight" "network" "time"];
    }
  ];

  nixMod.desktop = [
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

      services = {
        dwm-status.order = ["audio" "network" "time"];
        displayManager.sessionCommands = "xrandr --output DP-2 --mode 1920x1080 --rate 165.00";
      };
    }
  ];
}
