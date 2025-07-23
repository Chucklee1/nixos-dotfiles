{
  disko.devices = {
    disk.main = {
      type = "disk";
      device = "/dev/nvme0n1";
      content.type = "gpt";
      content.partitions.esp = {
        name = "ESP";
        size = "1G";
        type = "EF00";
        content = {
          type = "filesystem";
          format = "vfat";
          mountpoint = "/boot";
        };
      };
      content.partitions.fedora_boot = {
        size = "1G";
        type = "8300";
      };
      content.partitions.root = {
        size = "100%";
        content = {
          type = "btrfs";
          extraArgs = [ "-f" ]; 
          subvolumes = {
            "/nixos".mountpoint = "/";
            "/fedora" = { };
            "/nix" = {
              mountpoint = "/nix";
              mountOptions = [
                "compress=zstd"
                "noatime"
              ];
            };
          };
        };
      };
    };
  };
}
