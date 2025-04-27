{inputs, ...}: {
  nix.global = [
    {
      boot.loader = {
        efi.canTouchEfiVariables = true;
        grub = {
          enable = true;
          efiSupport = true;
          device = "nodev";
        };
      };
      services.displayManager.ly.enable = true;
    }
  ];

  nix.macbook = [
    inputs.impermanence.nixosModules.impermanence
    ({
      config,
      lib,
      ...
    }: {
      options.impermanence = {
        step1 = lib.mkEnableOption {default = builtins.elem "dm-snapshot" config.boot.initrd.kernelModules;};
        step2 = lib.mkEnableOption {default = false;};
      };
    })

    ({
      lib,
      config,
      ...
    }: {
      config = lib.mkIf config.impermenence.step1 {
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
      };
    })
    ({
      lib,
      config,
      ...
    }: {
      config = lib.mkIf config.impermenence.step2 {
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
          users.main = {
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
      };
    })
  ];
}
