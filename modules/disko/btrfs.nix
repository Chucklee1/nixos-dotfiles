{
  nix = [
    ({config, ...}: {
      disko.devices.disk.main = {
        type = "disk";
        device = config.disko.device;
        content.type = "gpt";
        content.partitions.ESP = {
          priority = 1;
          name = "ESP";
          end = "1G";
          type = "EF00";
          content = {
            type = "filesystem";
            format = "vfat";
            mountpoint = "/boot";
            mountOptions = ["umask=0077"];
          };
        };
        content.partitions.root = {
          size = "100%";
          content = {
            type = "btrfs";
            extraArgs = ["-f"];
            subvolumes = {
              "/nixos/root" = {
                mountpoint = "/";
                mountOptions = ["compress=zstd" "relatime"];
              };
              "/nixos/nix" = {
                mountpoint = "/nix";
                mountOptions = ["compress=zstd" "noatime"];
              };
            };
          };
        };
      };
    })
  ];
}
