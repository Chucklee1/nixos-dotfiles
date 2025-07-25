{
  disko.devices.disk.nvme = {
    type = "disk";
    device = "/dev/nvme0n1";
    content.type = "gpt";
    content.partitions = {
      ESP = {
        size = "1G"; # EFI system partition
        type = "EF00"; # EFI partition type
        content = {
          type = "filesystem";
          format = "vfat";
          mountpoint = "/boot";
          mountOptions = ["umask=0077"];
        };
      };
      zfs_root = {
        size = "remaining";
      };
    };
  };

  disko.devices.disk.sda = {
    type = "disk";
    device = "/dev/sda";
    content.type = "gpt";
    content.partitions = {
      zfs_root = {
        size = "remaining";
      };
    };
  };

  zpool = {
    rpool = {
      type = "zpool";
      options = {
        ashift = "12";
        autotrim = "on";
      };
      vdevs = [
        {
          type = "disk";
          devices = ["/dev/nvme0n1p2" "/dev/sda1"];
        }
      ];
      rootFsOptions = {
        acltype = "posixacl";
        canmount = "off";
        compression = "zstd";
        dnodesize = "auto";
        normalization = "formD";
        relatime = "on";
        xattr = "sa";
        "com.sun:auto-snapshot" = "false";
      };
      datasets = {
        root = {
          type = "zfs_fs";
          options.mountpoint = "none";
        };
        "root/emphereal" = {
          type = "zfs_fs";
          options.mountpoint = "legacy";
          mountpoint = "/";
          postCreateHook = "zfs snapshot rpool/root/empty@start";
        };
        "root/nix" = {
          type = "zfs_fs";
          options.mountpoint = "legacy";
          mountpoint = "/nix";
        };
        "root/srv" = {
          type = "zfs_fs";
          options.mountpoint = "legacy";
          mountpoint = "/srv";
        };
        "root/persist" = {
          type = "zfs_fs";
          options.mountpoint = "legacy";
          mountpoint = "/persist";
        };
      };
    };
  };
}
