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
    ({
      config,
      pkgs,
      user,
      ...
    }: let
      cfg = config.services.impermanence;
    in {
      fileSystems."/persist".neededForBoot = true;
      # nix-community/impermancence, issue 320, Author Doosty
      boot.initrd.systemd = {
        initrdBin = [
          pkgs.coreutils
          pkgs.btrfs-progs
        ];
        services.impermance-btrfs-rolling-root = {
          description = "Archiving existing BTRFS root subvolume and creating a fresh one";
          # Specify dependencies explicitly
          unitConfig.DefaultDependencies = false;
          # The script needs to run to completion before this service is done
          serviceConfig = {
            Type = "oneshot";
            # NOTE: to be able to see errors in your script do this
            # StandardOutput = "journal+console";
            # StandardError = "journal+console";
          };
          # This service is required for boot to succeed
          requiredBy = ["initrd.target"];
          # Should complete before any file systems are mounted
          before = ["sysroot.mount"];

          # Wait until the root device is available
          # If you're altering a different device, specify its device unit explicitly.
          # see: systemd-escape(1)
          requires = ["initrd-root-device.target"];
          after = [
            "initrd-root-device.target"
            # Allow hibernation to resume before trying to alter any data
            "local-fs-pre.target"
          ];

          # The body of the script. Make your changes to data here
          script = ''
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
        };
      };

      # disable initial sudo lecture
      security.sudo.extraConfig = ''
        Defaults lecture = never
      '';

      # user persistance stuff
      users.mutableUsers = false;
      environment.persistence."/persist" = {
        hideMounts = true;
        directories =
          [
            "/var/log"
            "/var/lib/bluetooth"
            "/var/lib/nixos"
            "/var/lib/systemd/coredump"
            "/etc/NetworkManager/system-connections"
          ]
          ++ cfg.persist.system.directories;

        files =
          [
            "/etc/machine-id"
          ]
          ++ cfg.persist.system.files;
        users.${user} = {
          directories =
            [
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
            ]
            ++ cfg.persist.user.directories;
          files = cfg.persist.user.files;
        };
      };
    })
  ];
}
