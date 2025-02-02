{
  config,
  lib,
  pkgs,
  inputs,
  system,
  def,
  ...
}: let
  mk = import ./libs.nix {
    inherit lib;
    modules = [
      "laptop"
      "desktop"
      "virt"
    ];
  };
in {
  options = mk.opts;

  config = lib.mkMerge [
    # -----------------------------------------------------------
    # machines
    # -----------------------------------------------------------
    (lib.mkIf config.laptop.enable {
      radeon.enable = true; # + enable gpuGlobal
    })

    (lib.mkIf config.desktop.enable {
      # external options
      nvidia.enable = true; # + enable gpuGlobal
      virt.enable = true;
      ntfs.enable = true;
      weylus.enable = true;

      # other drives
      fileSystems."/media/goat/BLUE_SATA" = {
        device = "/dev/disk/by-uuid/a6ffb4f9-049c-49a1-8b5f-1aca1b8dca08";
        fsType = "ext4";
      };
      networking.interfaces.enp7s0.useDHCP = lib.mkDefault true;
      networking.interfaces.wlp6s0.useDHCP = lib.mkDefault true;

      # extra games
      environment.systemPackages = with pkgs; [
        prismlauncher
        osu-lazer-bin
      ];
    })

    # -----------------------------------------------------------
    # globals - modules togs
    # -----------------------------------------------------------
    {
      ${def.host}.enable = true;
      nixvim.enable = true;
      steam.enable = true; # + enable wine
    }
    # -----------------------------------------------------------
    # globals - system options
    # -----------------------------------------------------------
    {
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
        extraSpecialArgs = {inherit system inputs def;};
        users.${def.username}.home = {
          stateVersion = "24.05"; # DO NOT CHANGE
          username = "${def.username}";
          homeDirectory = "/home/${def.username}";
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
    }
    # -----------------------------------------------------------
    # virtualisation
    # -----------------------------------------------------------
    (lib.mkIf config.virt.enable {
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
