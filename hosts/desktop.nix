{
  inputs,
  mod,
  ...
}:
with mod; {
  system = "x86_64-linux";
  builder = inputs.nixpkgs.lib.nixosSystem;
  user = "goat";
  modules = [
    programs.discord
    programs.emacs
    programs.nixvim
    programs.git
    programs.kitty
    programs.niri
    programs.dwm
    programs.obs
    programs.prismLauncher
    programs.rmpc
    programs.waybar
    programs.yazi
    programs.chromium
    programs.zen-browser

    hardware.cachyos-kernel
    hardware.uinput

    software.dev
    software.qol
    software.java
    software.rust
    software.texlive
    software.gaming

    system.boot
    system.home
    system.impermanence
    system.network
    system.pkgconfig
    system.sops
    system.sys-specs
    system.users

    services.flatpak
    services.graphical
    services.net-essentials
    services.nfs
    services.syncthing
    services.tailscale

    shell.fish
    shell.nushell
    shell.variables

    theming.blockgame
    theming.stylix

    virt.podman
    virt.qemu
  ];

  extraConfig = let
    WD = "ff111615-41d4-4836-8adc-c9374e08bee9";
    EFI = "5F4E-A7BB"; # on same device as WD
    EVO = "5a5dcb04-31cb-4fae-8fbe-1e8e83a83501";
    SEGATE = "0896390c-3618-46ea-a7cf-1f063694afb7";
    # passing fsType first to allow for specifying mkfs.fsType when calling
    # if there are no options, pass an empty list
    mkfs' = fsType: path: device: options: {
      fileSystems."${path}" =
        {
          inherit device fsType;
        }
        // (
          if (options == null)
          then {}
          else {inherit options;}
        );
    };

    mkfs = {
      btrfs = path: uuid: options: mkfs' "btrfs" path "/dev/disk/by-uuid/${uuid}" options;
      ext4 = path: uuid: options: mkfs' "ext4" path "/dev/disk/by-uuid/${uuid}" options;
      vfat = path: uuid: options: mkfs' "vfat" path "/dev/disk/by-uuid/${uuid}" options;
      nfs = path: device: options: mkfs' "nfs" path device options;
    };
  in [
    (mkfs.vfat "/boot/efi" EFI ["fmask=0022" "dmask=0022"])
    (mkfs.btrfs "/boot" WD ["subvol=WD/nixos/boot" "noatime"])
    (mkfs.btrfs "/persist" WD ["subvol=WD/nixos/persist" "noatime" "compress=zstd"])
    (mkfs.btrfs "/nix" WD ["subvol=WD/nix" "noatime" "compress=zstd"])

    (mkfs.btrfs "/" WD ["subvol=WD/nixos/root" "relatime" "compress=zstd"])

    (mkfs.btrfs "/opt" EVO ["subvol=EVO/opt" "noatime"])
    (mkfs.btrfs "/opt/Games" EVO ["subvol=EVO/Games" "noatime"])
    (mkfs.btrfs "/opt/Steam" EVO ["subvol=EVO/Steam" "noatime"])

    (mkfs.btrfs "/srv" EVO ["subvol=EVO/srv" "relatime" "compress=zstd"])

    (mkfs.btrfs "/.snapshots/WD" WD ["subvol=WD/.snapshots" "noatime" "compress=zstd"])
    (mkfs.btrfs "/.snapshots/EVO" EVO ["subvol=EVO/.snapshots" "noatime" "compress=zstd"])

    (mkfs.ext4 "/srv/Pictures" SEGATE null)

    ({lib, config, pkgs, user, ...}: {
      swapDevices = [];
      boot.loader.efi.efiSysMountPoint = "/boot/efi";

      # kernel
      boot.initrd.availableKernelModules = [
        "xhci_pci"
        "ahci"
        "nvme"
        "usbhid"
        "usb_storage"
        "sd_mod"
      ];
      boot.initrd.kernelModules = [
        "vfio_pci"
        "vfio"
        "vfio_iommu_type1"
        "ntsync"
      ];

      boot.supportedFilesystems = ["btrfs" "ext4" "ntfs"];

      # vm stuff
      boot.kernelModules = [
        "kvm"
        "kvmfr"
        "kvm_amd"
      ];
      boot.kernelParams = [
        "amd_iommu=on"
        "iommu=pt"
        "video=efifb:off"
        "vfio-pci.ids=10de:2503,10de:228e"
      ];
      boot.blacklistedKernelModules = [
        "nouveau"
        "nvidia"
        "nvidia_drm"
        "nvidia_modeset"
        "nvidiafb"
      ];

      boot.extraModulePackages = [config.boot.kernelPackages.kvmfr];
      boot.extraModprobeConfig = ''
        options kvmfr static_size_mb=32
      '';

      services.udev.extraRules = ''
          SUBSYSTEM=="kvmfr", OWNER="${user}", GROUP="kvm", MODE="0660"
      '';

      virtualisation.libvirtd.qemu.verbatimConfig = ''
          cgroup_device_acl = [
              "/dev/null", "/dev/full", "/dev/zero",
              "/dev/random", "/dev/urandom",
              "/dev/ptmx", "/dev/kvm",
              "/dev/kvmfr0"
          ]
      '';

      environment.systemPackages = [pkgs.looking-glass-client];

      # cpu
      hardware.cpu.amd.updateMicrocode = true;
      hardware.enableRedistributableFirmware = true;

      # gpu
      services.xserver.videoDrivers = lib.mkForce ["amdgpu"];

      # monitor res on xserver
      services.xserver.monitorSection = ''
        Identifier "DisplayPort-0"
        Option "PreferredMode" "1920x1080_165.00"
      '';

      boot.loader.grub.useOSProber = true;
      # dual boot entry so I dont need a seperate bootloader for arch
      boot.loader.grub.extraEntries = ''
        menuentry "arch" {
            insmod btrfs
            search --no-floppy --fs-uuid --set=root ${WD}
            linux /WD/arch/boot/vmlinuz-linux root=UUID=${WD} rw rootflags=subvol=WD/arch/root
            initrd /WD/arch/boot/initramfs-linux.img
        }
      '';
    })
    # impermenance setup
    {
      # persistance stuff
      services.impermanence = {
        device = "/dev/disk/by-label/WD";
        root = {
          target = "WD/nixos/root";
          blank = "WD/nixos/root_blank";
        };
        persist = {
          system.directories = [
            "/var/lib/tailscale"
            "/var/lib/libvirt/images"
          ];
          user.directories = [
            ".cache/zen"
            ".config/discord"
            ".config/listenbrainz-mpd"
            ".config/sunshine"
            ".config/zen"
            ".config/openmw"
            ".local/share/direnv"
            ".local/share/mpd"
            ".local/share/openmw"
            ".local/state/syncthing"
            ".local/share/zoxide"
            ".var"
            ".ollama"
            ".factorio"
          ];
        };
      };
    }
    # nix ld
    {programs.nix-ld.enable = true;}
    # sops
    ({
      config,
      user,
      ...
    }: {
      sops.age.keyFile = "/persist/secrets/age.keys.txt";
      users.users.${user}.hashedPasswordFile = config.sops.secrets."gregtrain/goat".path;
    })
    ({
      lib,
      user,
      ...
    }: {
      # symlink setup on login
      home-manager.users.${user} = {
        programs.fish.loginShellInit = ''
          sln $HOME/Repos/nixos-dotfiles $HOME/
          sln $HOME/Repos/nixos-dotfiles/pkgs/emacs/config.el $HOME/.emacs.d/init.el
          sln $HOME/Repos/nixos-dotfiles/pkgs/emacs/snippets $HOME/.emacs.d/

          sln /srv/Pictures $HOME/

          sln /opt/Steam $HOME/.local/share/
          for it in /opt/Games/*
            sln $it $HOME/.local/share/
          end
        '';
        # monitor configuration
        programs.niri.settings = {
          # must use {} since niri does not like "key = function -float;"
          input.mouse = lib.mkForce {accel-speed = -0.75;};
          # no clue why my monitor has so many 0's...
          outputs = {
            "HKC OVERSEAS LIMITED 24E4 0000000000001" = {
              mode = {
                width = 1920;
                height = 1080;
                refresh = 165.001;
              };
            };
          };
        };
      };
    })
  ];
}
