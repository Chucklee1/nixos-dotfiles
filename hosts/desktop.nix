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
    system.users
    system.network
    system.pkgconfig
    system.sys-specs

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
    inputs.impermanence.nixosModules.impermanence
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

    ({
      lib,
      pkgs,
      user,
      ...
    }: {
      swapDevices = [];
      fileSystems."/persist".neededForBoot = true;
      boot.loader.efi.efiSysMountPoint = "/boot/efi";
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

      # kernel
      boot.initrd.availableKernelModules = ["xhci_pci" "ahci" "nvme" "usbhid" "usb_storage" "sd_mod"];
      boot.kernelModules = ["kvm" "kvm_amd" "ntsync"];
      boot.supportedFilesystems = ["btrfs" "ext4" "ntfs"];
      boot.initrd.postResumeCommands = lib.mkAfter ''
        mkdir -p /mnt
        mount -o subvolid=5 /dev/disk/by-label/WD /mnt

        btrfs subvolume list -o /mnt/WD/nixos/root |
        cut -f9 -d' ' |
        while read subvolume; do
            echo "deleting $subvolume subvolume..."
            btrfs subvolume delete "/mnt/$subvolume"
        done &&
        echo "deleting nixos/root subvolume..." &&
        btrfs subvolume delete /mnt/WD/nixos/root

        echo "restoring blank nix/root subvolume..."
        btrfs subvolume snapshot /mnt/WD/nixos/root_blank /mnt/WD/nixos/root

        umount /mnt
      '';

      # cachyos kernel
      nix.settings.substituters = ["https://cache.garnix.io"];
      nix.settings.trusted-public-keys = ["cache.garnix.io:CTFPyKSLcx5RMJKfLo5EEPUObbA78b0YQ2DTCJXqr9g="];
      nixpkgs.overlays = [inputs.nix-cachyos-kernel.overlays.pinned];
      boot.kernelPackages = pkgs.cachyosKernels.linuxPackages-cachyos-latest;

      # cpu
      hardware.cpu.amd.updateMicrocode = true;
      hardware.enableRedistributableFirmware = true;

      # gpu
      services.xserver.videoDrivers = ["amdgpu"];

      # user persistance stuff
      users.mutableUsers = false;
      users.users.${user}.hashedPasswordFile = "/persist/passwords/goat";
      environment.persistence."/persist" = {
        hideMounts = true;
        directories = [
          "/var/log"
          "/var/lib/bluetooth"
          "/var/lib/nixos"
          "/var/lib/systemd/coredump"
          "/var/lib/tailscale"
          "/etc/NetworkManager/system-connections"
        ];
        files = [
          "/etc/machine-id"
        ];
        users.${user} = {
          directories = [
            "Downloads"
            "Documents"
            "Videos"
            "Repos"
            {
              directory = ".ssh";
              mode = "0700";
            }
            {
              directory = ".local/share/keyrings";
              mode = "0700";
            }
            ".local/share/direnv"
            ".local/share/zoxide"
            ".local/state/syncthing"
            ".cache/zen"
            ".config/zen"
            ".config/discord"
            ".var"
          ];
        };
      };
    })
    # monitor configuration
    ({
      lib,
      user,
      ...
    }: {
      home-manager.users.${user} = {
        programs.fish.loginShellInit = ''
          set -g dotfiles $HOME/Repos/nixos-dotfiles
          set -g emacs_dir $HOME/.emacs.d

          # symlink dotfiles to user root
          if not test -d $HOME/nixos-dotfiles
              echo "symlinking $dotfiles to $HOME"
              ln -s $dotfiles $HOME
          else
              echo "$HOME/nixos-dotfiles already exist"
          end

          if not test -d $emacs_dir; mkdir $emacs_dir; end
          if test -d $emacs_dir/snippets; rm -rf $emacs_dir/snippets; end
          ln -s $dotfiles/pkgs/emacs/snippets $emacs_dir/

          # same for emacs config
          if not test -f $emacs_dir/init.el
              echo "symlinking $dotfiles/pkgs/emacs/config.el to $emacs_dir/init.el"
              ln -s $dotfiles/pkgs/emacs/config.el $emacs_dir/init.el
          else
              echo "$emacs_dir/init.el already exist"
          end

          set -e dotrepo
          set -e emacs_dir

          # opt symlinks
          ln -s /opt/Steam $HOME/.local/share/
          for file in /opt/Games*; ln -s $file $HOME/.local/share/; end
          ln -s /srv/Games $HOME/

          # srv symlinks
          ln -s /srv/Pictures $HOME/
        '';
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
            "Shenzhen KTC Technology Group H27T27 Unknown" = {
              scale = 1.2;
              mode = {
                width = 2560;
                height = 1440;
                refresh = 99.965;
              };
            };
          };
        };
      };
    })
  ];
}
/*
unused but may use later section
 # only needed if nvidia is the only host card
 environment.variables = {
   LIBVA_DRIVER_NAME = "nvidia";
   NVD_BACKEND = "direct";
   GBM_BACKEND = "nvidia-drm";
   __GLX_VENDOR_LIBRARY_NAME = "nvidia";
 };
*/
