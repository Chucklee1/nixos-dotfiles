{
  main ? throw "error: proper argument is --argstr main /dev/diskName (with single then double wrapped qoutes for device arg /dev/diskName)",
  home ? throw "error: proper argument is --argstr home /dev/diskName (with single then double wrapped qoutes for device arg /dev/diskName)",
  ...
}:
{
  disko.devices.disk.main = {
    device = main;
    type = "disk";
    content = {
      type = "gpt";
      partitions = {
        ESP = {
          type = "EF00";
          size = "1G";
          content = {
            type = "filesystem";
            format = "vfat";
            mountpoint = "/boot";
            mountOptions = [ "umask=0077" ];
          };
        };
        root = {
          size = "100%";
          content = {
            type = "filesystem";
            format = "ext4";
            mountpoint = "/";
          };
        };
      };
    };
  };
	disko.devices.disk.home = {
		device = home;
		type = "disk";
		content = {
			type = "gpt";
			partitions.home = {
				size = "100%";
				content = {
					type = "filesystem";
					format = "ext4";
					mountpoint = "/home";
				};
			};
		};
	};
}
