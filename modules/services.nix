{inputs, ...}: {
  #services.navidrome.enable = true;
  nix.global = [
    # general drivers
    ({
      lib,
      pkgs,
      ...
    }: {
      hardware.cpu.amd.updateMicrocode = lib.mkDefault true;
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
    # net related
    {
      # polkit
      security.polkit.enable = true;

      # ssh
      services.openssh = {
        enable = true;
        settings = {
          PasswordAuthentication = false;
          PermitRootLogin = "prohibit-password";
        };
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
      hardware = {
        bluetooth.enable = true;
        bluetooth.powerOnBoot = true;
      };
      services.blueman.enable = true;

      # misc
      services = {
        displayManager.ly.enable = true;
        printing.enable = true;
        fstrim.enable = true;
        tumbler.enable = true;
        gvfs.enable = true;
      };
    }
  ];

  nix.laptop = [
    {
      services.xserver.videoDrivers = ["amdgpu"];
      hardware.amdgpu.amdvlk.enable = true;

      services = {
        navidrome = {
          enable = true;
          openFirewall = true;
          settings.MusicFolder = "/run/media/goat/T7";
        };
      };
    }
  ];

  nix.desktop = [
    # tablet support
    {
      users.users."goat".extraGroups = ["uinput"];
      hardware.uinput.enable = true;
      programs.weylus.enable = true;
      services.udev.extraRules = ''
        KERNEL=="uinput", MODE="0660", GROUP="uinput", OPTIONS+="static_node=uinput"
      '';
    }
    # nvidia
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
        # wayland
        GBM_BACKEND = "nvidia-drm";
        __GLX_VENDOR_LIBRARY_NAME = "nvidia";
      };
    })
    # virtualisation
    ({pkgs, ...}: {
      users.users."goat".extraGroups = ["libvirtd"];
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
    })
  ];

  home.desktop = [
    {
      dconf.settings."org/virt-manager/virt-manager/connections" = {
        autoconnect = ["qemu:///system"];
        uris = ["qemu:///system"];
      };
    }
  ];
}
