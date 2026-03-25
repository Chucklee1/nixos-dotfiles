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
    net.nfs
    net.syncthing
    net.tailscale

    programs.discord
    programs.emacs
    programs.nixvim
    programs.flatpak
    programs.git
    programs.kitty
    programs.niri
    programs.obs
    programs.prismLauncher
    programs.rmpc
    programs.waybar
    programs.yazi
    programs.chromium
    programs.zen-browser

    software.dev
    software.qol
    software.java
    software.rust
    software.texlive
    software.gaming

    system.boot
    system.home
    system.impermance
    system.network
    system.pkgconfig
    system.sops
    system.sys-specs
    system.users

    services.graphical
    services.fcitx

    shell.fish
    shell.nushell
    shell.variables

    theming.blockgame
    theming.stylix
    theming.themes.nord
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

    ({pkgs, ...}: {
      swapDevices = [];
      boot.loader.efi.efiSysMountPoint = "/boot/efi";

      # kernel
      boot.initrd.availableKernelModules = ["xhci_pci" "ahci" "nvme" "usbhid" "usb_storage" "sd_mod"];
      boot.kernelModules = ["kvm" "kvm_amd" "ntsync"];
      boot.supportedFilesystems = ["btrfs" "ext4" "ntfs"];

      # cachyos kernel
      nix.settings.substituters = ["https://cache.garnix.io"];
      nix.settings.trusted-public-keys = ["cache.garnix.io:CTFPyKSLcx5RMJKfLo5EEPUObbA78b0YQ2DTCJXqr9g="];
      nixpkgs.overlays = [inputs.nix-cachyos-kernel.overlays.pinned];
      boot.kernelPackages = pkgs.cachyosKernels.linuxPackages-cachyos-latest;

      # cpu
      hardware.cpu.amd.updateMicrocode = true;
      hardware.enableRedistributableFirmware = true;

      # gpu
      services.xserver.videoDrivers = ["amdgpu" "nvidia"];
      hardware.nvidia = {
        modesetting.enable = true;
        powerManagement.enable = false;
        powerManagement.finegrained = false;
        open = false;
      };
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
          system.directories = ["/var/lib/tailscale"];
          user.directories = [
            ".cache/zen"
            ".config/discord"
            ".config/listenbrainz-mpd"
            ".config/zen"
            ".local/share/direnv"
            ".local/share/mpd"
            ".local/state/syncthing"
            ".local/share/zoxide"
            ".var"
          ];
        };
      };
    }
    # sops
    ({config, user, ...}: {
      sops.age.keyFile = "/persist/secrets/age.keys.txt";
      users.users.${user}.hashedPasswordFile = config.sops.secrets."gregtrain/goat".path;
    })
    # symlink setup on login
    ({
      lib,
      user,
      ...
    }: {
      home-manager.users.${user} = {
        programs.fish.loginShellInit = ''
          sln $HOME/Repos/nixos-dotfiles $HOME/
          sln $HOME/Repos/nixos-dotfiles/pkgs/emacs/config.el $HOME/.emacs.d/init.el
          sln $HOME/Repos/nixos-dotfiles/pkgs/emacs/snippets $HOME/.emacs.d/

          sln /srv/Pictures $HOME/Pictures

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
/*
for later if needed section
boot.loader.grub.useOSProber = true;
# dual boot entry so I dont need a seperate bootloader for arch
boot.loader.grub.extraEntries = ''
menuentry "arch" {
    insmod btrfs
    search --no-floppy --fs-uuid --set=root ${WD}
    linux /WD/arch/boot/vmlinuz-linux root=UUID=${WD} rw rootflags=subvol=WD/arch
    initrd /WD/arch/boot/initramfs-linux.img
}
'';
*/
