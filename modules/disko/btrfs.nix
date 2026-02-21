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
          end = "512M";
          type = "EF00";
          content = {
            type = "filesystem";
            format = "vfat";
            mountpoint = "/boot/efi";
            mountOptions = ["fmask=0022" "dmask=0022"];
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
              "/nixos/boot" = {
                mountpoint = "/boot";
                mountOptions = ["noatime"];
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
