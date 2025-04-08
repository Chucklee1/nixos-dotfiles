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
        kernelModules = ["kvm-amd"];
        supportedFilesystems = ["ntfs"];
      };
      hardware.cpu.amd.updateMicrocode = true;
    }
  ];

  nix.laptop = [
    {
      boot = {
        initrd.availableKernelModules = ["nvme" "xhci_pci" "usb_storage" "sd_mod"];
        initrd.kernelModules = [];
        kernelModules = ["kvm-amd"];
        extraModulePackages = [];
      };
      hardware.cpu.amd.updateMicrocode = true;
    }
  ];

  nix.macbook = [
    inputs.disko.nixosModules.default
    {
      disko.devices = {
        disk.main = {
          device = "/dev/sda";
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
    ({modulesPath, ...}: {
      imports = [(modulesPath + "/hardware/network/broadcom-43xx.nix")];

      boot = {
        initrd.availableKernelModules = ["xhci_pci" "ahci" "usbhid" "usb_storage" "sd_mod"];
        initrd.kernelModules = ["dm-snapshot"];
        kernelModules = ["kvm-intel"];
        extraModulePackages = [];
      };
      hardware.cpu.intel.updateMicrocode = true;
    })
    inputs.impermanence.nixosModules.impermanence
    ({lib, ...}: {
      boot.initrd.postDeviceCommands = lib.mkAfter ''
        mkdir /btrfs_tmp
        mount /dev/root_vg/root /btrfs_tmp
        if [[ -e /btrfs_tmp/root ]]; then
            mkdir -p /btrfs_tmp/old_roots
            timestamp=$(date --date="@$(stat -c %Y /btrfs_tmp/root)" "+%Y-%m-%-d_%H:%M:%S")
            mv /btrfs_tmp/root "/btrfs_tmp/old_roots/$timestamp"
        fi

        delete_subvolume_recursively() {
            IFS=$'\n'
            for i in $(btrfs subvolume list -o "$1" | cut -f 9- -d ' '); do
                delete_subvolume_recursively "/btrfs_tmp/$i"
            done
            btrfs subvolume delete "$1"
        }

        for i in $(find /btrfs_tmp/old_roots/ -maxdepth 1 -mtime +30); do
            delete_subvolume_recursively "$i"
        done

        btrfs subvolume create /btrfs_tmp/root
        umount /btrfs_tmp
      '';

      fileSystems."/persist".neededForBoot = true;
      environment.persistence."/persist/system" = {
        hideMounts = true;
        directories = [
          "/var/log"
          "/var/lib/bluetooth"
          "/var/lib/nixos"
          "/var/lib/systemd/coredump"
          "/etc/NetworkManager/system-connections"
        ];
      };

      programs.fuse.userAllowOther = true;
    })
  ];
  home.macbook = [
    inputs.impermanence.nixosModules.home-manager.impermanence
    {
      home.persistence."/persist/home" = {
        allowOther = true;
        directories = [
          "nixos-dotfiles"
          "Navidrome"
          "Downloads"
          "Music"
          "Pictures"
          "Documents"
          ".ssh"
          ".config/chromium"
          ".cache/chromium"
          ".local/share/keyrings"
        ];
      };
    }
  ];
}
