{inputs}: let
  mkFs = path: device: fsType: options: {
    fileSystems.${path} =
      {inherit device fsType;}
      // (
        if options == null
        then {}
        else {inherit options;}
      );
  };
in {
  nix.global = [
    ({modulesPath, ...}: {
      imports = [(modulesPath + "/installer/scan/not-detected.nix")];
    })
  ];

  nix.desktop = [
    (mkFs "/" "/dev/disk/by-uuid/96c41aaf-846f-47b1-8319-eed5a3a32294" "ext4" null)
    (mkFs "/boot" "/dev/disk/by-uuid/75D4-A9F7" "vfat" ["fmask=0022" "dmask=0022"])
    (mkFs "/media/goat/BLUE_SATA" "/dev/disk/by-uuid/a6ffb4f9-049c-49a1-8b5f-1aca1b8dca08" "ext4" null)
    {
      swapDevices = [];
      boot = {
        initrd.availableKernelModules = ["nvme" "xhci_pci" "ahci" "usb_storage" "usbhid" "sd_mod"];
        initrd.kernelModules = [];
        extraModulePackages = [];
        supportedFilesystems = ["ntfs"];
      };
    }
  ];

  nix.laptop = [
    inputs.disko.nixosModules.default
    {
      disko.devices = {
        disk.main = {
          device = "/dev/nvme0n1";
          type = "disk";
          content = {
            type = "gpt";
            partitions = {
              boot = {
                name = "boot";
                size = "1M";
                type = "EF02";
              };
              esp = {
                name = "ESP";
                size = "500M";
                type = "EF00";
                content = {
                  type = "filesystem";
                  format = "vfat";
                  mountpoint = "/boot";
                };
              };
              root = {
                name = "root";
                size = "100%";
                content = {
                  type = "lvm_pv";
                  vg = "root_vg";
                };
              };
            };
          };
        };
        lvm_vg = {
          root_vg = {
            type = "lvm_vg";
            lvs = {
              root = {
                size = "100%FREE";
                content = {
                  type = "btrfs";
                  extraArgs = ["-f"];

                  subvolumes = {
                    "/root" = {
                      mountpoint = "/";
                    };

                    "/persist" = {
                      mountOptions = ["subvol=persist" "noatime"];
                      mountpoint = "/persist";
                    };

                    "/nix" = {
                      mountOptions = ["subvol=nix" "noatime"];
                      mountpoint = "/nix";
                    };
                  };
                };
              };
            };
          };
        };
      };
    }
    {
      boot.initrd.availableKernelModules = ["nvme" "xhci_pci" "usb_storage" "sd_mod"];
      boot.initrd.kernelModules = [];
      boot.extraModulePackages = [];
    }
  ];
}
