{device ? throw "DEV NOT FOUND (try /dev/sda format)", ...}: {
  disko.devices.disk.main = {
    device = device;
    type = "disk";
    content.type = "gpt";
    content.partitions.ESP = {
      type = "EF00";
      size = "1G";
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
        type = "filesystem";
        format = "ext4";
        mountpoint = "/";
      };
    };
  };
}
