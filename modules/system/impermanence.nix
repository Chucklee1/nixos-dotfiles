{inputs, ...}: {
  nix = [
    inputs.impermanence.nixosModules.impermanence
    ({lib, ...}: {
      options.services.impermanence = with lib; {
        device = mkOption {
          type = types.str;
          description = "Path to the target impermanent device (eg /dev/vda)";
        };
        root = {
          target = mkOption {
            type = types.str;
            description = ''
              Path to the emphereal btrfs subvolume root
              with no trailing / (eg @root)
              THIS OPTION MUST BE SET IF USING THIS MODULE
            '';
          };
          blank = mkOption {
            type = types.str;
            description = ''
              Path to the blank btrfs subvolume that will replace the
              existing subvolume on each subsiquent reboot
              also with no trailing / (eg @blank_root)
              THIS OPTION MUST BE SET IF USING THIS MODULE
            '';
          };
        };
        persist = {
          system = {
            directories = mkOption {
              type = types.listOf types.anything;
              default = [];
              description = ''
                additional system-level directories to perserve across reboots
                added entries from this option will be appended to
                environment.persistence."/persist".directories
              '';
            };
            files = mkOption {
              type = types.listOf types.anything;
              default = [];
              description = ''
                additional system-level files to perserve across reboots
                added entries from this option will be appended to
                environment.persistence."/persist".files
              '';
            };
          };
          user = {
            directories = mkOption {
              type = types.listOf types.anything;
              default = [];
              description = ''
                same as system.directories, but will append entires to
                environment.persistence."/persist".users.username.directories
              '';
            };
            files = mkOption {
              type = types.listOf types.anything;
              default = [];
              description = ''
                same as system.files, but will append entires to
                environment.persistence."/persist".users.username.files
              '';
            };
          };
        };
      };
    })
    ({lib, config, user, ...}: let
      cfg = config.services.impermanence;
    in {
      fileSystems."/persist".neededForBoot = true;
      boot.initrd.postResumeCommands = lib.mkAfter ''
        mkdir -p /mnt
        mount -o subvolid=5 ${cfg.device} /mnt

        btrfs subvolume list -o /mnt/${cfg.root.target} |
        cut -f9 -d' ' |
        while read subvolume; do
            echo "deleting $subvolume subvolume..."
            btrfs subvolume delete "/mnt/$subvolume"
        done &&
        echo "deleting ${cfg.root.target} subvolume..." &&
        btrfs subvolume delete /mnt/${cfg.root.target}

        echo "restoring blank ${cfg.root.target} subvolume..."
        btrfs subvolume snapshot /mnt/${cfg.root.blank} /mnt/${cfg.root.target}

        umount /mnt
      '';

      # disable initial sudo lecture
      security.sudo.extraConfig = ''
        Defaults lecture = never
      '';

      # user persistance stuff
      users.mutableUsers = false;
      environment.persistence."/persist" = {
        hideMounts = true;
        directories = [
          "/var/log"
          "/var/lib/bluetooth"
          "/var/lib/nixos"
          "/var/lib/systemd/coredump"
          "/etc/NetworkManager/system-connections"
        ] ++ cfg.perserve.system.directories;

        files = [
          "/etc/machine-id"
        ] ++ cfg.perserve.system.files;
        users.${user} = {
          directories = [
            "Downloads"
            "Documents"
            "Repos"
            {
              directory = ".ssh";
              mode = "0700";
            }
            {
              directory = ".local/share/keyrings";
              mode = "0700";
            }
          ] ++ cfg.perserve.user.directories;
          files = cfg.perserve.user.files;
        };
      };
    })
  ];
}
