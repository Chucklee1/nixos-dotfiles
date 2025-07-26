{
  inputs,
  self,
  ...
}: {
  desktop.nix = [
    inputs.disko.nixosModules.default
    (import "${self}/assets/disko/desktop.nix")
    /*
    inputs.impermanence.nixosModules
    ({pkgs, ...}: {
      boot.initrd.systemd = {
        enable = true;
        services.initrd-rollback-root = {
          after = ["zfs-import-rpool.service"];
          wantedBy = ["initrd.target"];
          before = ["sysroot.mount"];
          path = [pkgs.zfs];
          description = "Rollback root fs";
          unitConfig.DefaultDependencies = "no";
          serviceConfig.Type = "oneshot";
          script = "zfs rollback -r rpool/root/emphereal@start";
        };
      };
    })
    {
      environment.persistence."/persist" = {
        hideMounts = true;
        directories = [
          "/var/log"
          "/var/lib/bluetooth"
          "/var/lib/nixos"
          "/var/lib/systemd/coredump"
          "/etc/NetworkManager/system-connections"
        ];
        files = ["/etc/machine-id"];
        users.goat.directories =
          map (directory: {
            inherit directory;
            mode = "0700";
          }) [
            ".ssh"
            "Repos"
            "Documents"
            "Downloads"
            ".mozilla"
            ".librewolf"
            ".local/share/keyrings"
            ".local/share/direnv"
          ];
      };
    }
    */
  ];

  laptop.nix = [
    inputs.disko.nixosModules.default
    (import "${self}/assets/disko/ext4.nix" {device = "/dev/nvme0n1";})
    {
      boot.initrd.availableKernelModules = ["nvme" "xhci_pci" "ahci" "usb_storage" "sd_mod"];
      boot.kernelModules = ["kvm-intel"];
      boot.supportedFilesystems = ["ntfs"];
      hardware.cpu.intel.updateMicrocode = true;
      hardware.enableRedistributableFirmware = true;
    }
  ];

  umbra.nix = [
    ({modulesPath, ...}: {
      imports = [(modulesPath + "/installer/cd-dvd/installation-cd-minimal.nix")];
    })
  ];
}
