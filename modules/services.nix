{
  nix.global = [
    # general services
    {
      security.polkit.enable = true;

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
    # net related
    {
      # ssh
      services.openssh = {
        enable = true;
        settings = {
          PasswordAuthentication = false;
          PermitRootLogin = "prohibit-password";
        };
      };
    }
  ];

  nix.laptop = [
    {
      services = {
        tailscale = {
          enable = true;
          port = 3030;
          useRoutingFeatures = "server";
        };
        navidrome = {
          enable = true;
          openFirewall = true;
          settings = {
            Address = "100.98.210.96";
            MusicFolder = "/run/media/goat/T7/music";
          };
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
