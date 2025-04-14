{inputs, ...}: {
  nix.global = [
    {
      boot = {
        loader.grub = {
          enable = true;
          efiSupport = true;
          device = "nodev";
        };
      };
      # display manager
      services.displayManager.ly.enable = true;
    }
  ];

  nix.desktop = [{boot.loader.efi.canTouchEfiVariables = true;}];

  nix.nimbus = [
    inputs.impermanence.nixosModules.impermanence
    ({lib, ...}: {
      boot.loader.grub.efiInstallAsRemovable = lib.mkDefault true;

      users.users.main = {
        initialPassword = "password";
      };

      boot.initrd.postDeviceCommands = lib.mkAfter ''
        mkdir /btrfs_tmp
        mount /dev/root_vg/root /btrfs_tmp
        if [[ -e /btrfs_tmp/root ]]; then
            mkdir -p /btrfs_tmp/old_roots
            timestamp=$(date --date="@$(stat -c %Y /btrfs_tmp/root)" "+%Y-%m-%-d_%H:%M:%S")
            mv /btrfs_tmp/root "/btrfs_tmp/old_roots/$timestamp"
        fi

        delete_subvolume_recursively() {
            IFS=$'\n'
            for i in $(btrfs subvolume list -o "$1" | cut -f 9- -d ' '); do
                delete_subvolume_recursively "/btrfs_tmp/$i"
            done
            btrfs subvolume delete "$1"
        }

        for i in $(find /btrfs_tmp/old_roots/ -maxdepth 1 -mtime +30); do
            delete_subvolume_recursively "$i"
        done

        btrfs subvolume create /btrfs_tmp/root
        umount /btrfs_tmp
      '';

      fileSystems."/persist".neededForBoot = true;
      environment.persistence."/persist/system" = {
        hideMounts = true;
        directories = [
          "/etc/nixos"
          "/var/log"
          "/var/lib/bluetooth"
          "/var/lib/nixos"
          "/var/lib/systemd/coredump"
          "/etc/NetworkManager/system-connections"
          #{ directory = "/var/lib/colord"; user = "colord"; group = "colord"; mode = "u=rwx,g=rx,o="; }
        ];
        files = [
          #{ file = "/var/keys/secret_file"; parentDirectory = { mode = "u=rwx,g=,o="; }; }
        ];
        users.talyz = {
          directories = [
            "Downloads"
            "Music"
            "Pictures"
            "Documents"
            "nixos-dotfiles"
            {
              directory = ".ssh";
              mode = "0700";
            }
            {
              directory = ".local/share/keyrings";
              mode = "0700";
            }
            ".local/share/direnv"
          ];
          files = [
            #".screenrc"
          ];
        };
      };

      programs.fuse.userAllowOther = true;
    })
  ];
}
