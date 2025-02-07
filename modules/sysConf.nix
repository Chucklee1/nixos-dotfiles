{home-manager, ...}: {
  global.nix = [
    home-manager.nixosModules.home-manager
    ({
      lib,
      config,
      def,
      ...
    }: {
      # boot
      boot = {
        initrd.systemd.enable = true; # force systemd to load early
        loader = {
          efi.canTouchEfiVariables = true;
          grub = {
            enable = true;
            efiSupport = true;
            device = "nodev";
          };
        };
      };

      # system options
      system.stateVersion = "24.05";
      networking = {
        networkmanager.enable = true;
        hostName = "${def.host}-${def.username}";
      };
      i18n.defaultLocale = "en_CA.UTF-8";
      time.timeZone = "America/Vancouver";
      console = {
        earlySetup = true;
        keyMap = "us";
      };

      # nix options
      nixpkgs = {
        hostPlatform = lib.mkDefault "x86_64-linux";
        config.allowUnfree = true;
      };
      nix.settings = {
        auto-optimise-store = true;
        experimental-features = ["nix-command" "flakes"];
      };

      # user
      users.users.${def.username} = {
        isNormalUser = true;
        extraGroups = [
          "wheel"
          "networkmanager"
          "uinput"
          "libvirtd"
          "audio"
          "video"
        ];
      };

      home-manager = {
        useGlobalPkgs = true;
        useUserPackages = true;
        users.${def.username}.home = {
          stateVersion = "24.05"; # DO NOT CHANGE
          username = "${def.username}";
          homeDirectory = "/home/${def.username}";
          imports = config._module.args.home;
        };
      };

      # security & polkit
      security = {
        polkit.enable = true;
        rtkit.enable = true; # for sound
      };
      services.openssh = {
        enable = true;
        settings = {
          PasswordAuthentication = false;
          PermitRootLogin = "prohibit-password";
        };
      };

      # global drivers

      # audio
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
    })
  ];

  desktop.nix = [
    ({pkgs, ...}: {
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
    })
  ];
}
