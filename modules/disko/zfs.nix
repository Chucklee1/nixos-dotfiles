{
  nix = [({config, ...}: {
    disko.devices = {
      disk.root = {
        type = "disk";
        device = config.disko.device;
        content = {
          type = "gpt";
          partitions.ESP = {
            size = "1G";
            type = "EF00";
            content = {
              type = "filesystem";
              format = "vfat";
              mountpoint = "/boot";
              mountOptions = ["umask=0077"];
            };
          };
          partitions.zfs = {
            size = "100%";
            content = {
              type = "zfs";
              pool = "rpool";
            };
          };
        };
      };
      zpool.rpool = {
        type = "zpool";
        mode = "";
        # Workaround: cannot import 'rpool': I/O error in disko tests
        options.cachefile = "none";
        rootFsOptions = {
          compression = "lz4";
          atime = "off";
          xattr = "sa";
          "com.sun:auto-snapshot" = "true";
        };
        postCreateHook = "zfs list -t snapshot -H -o name | grep -E '^rpool@blank$' || zfs snapshot rpool@blank";

        datasets = {
          root = {
            type = "zfs_fs";
            mountpoint = "/";
          };
          nix = {
            type = "zfs_fs";
            mountpoint = "/nix";
          };
          home = {
            type = "zfs_fs";
            mountpoint = "/home";
            options.relatime = "on";
          };
          opt = {
            type = "zfs_fs";
            mountpoint = "/opt";
          };
          srv = {
            type = "zfs_fs";
            mountpoint = "/srv";
            options = {
              compression = "zstd";
              recordsize = "1M";
              xattr = "on";
              "com.sun:auto-snapshot" = "false";
            };
          };
        };
      };
    };
  })];
}
