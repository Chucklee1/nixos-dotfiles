{
  inputs,
  self,
  ...
}: {
  nix.global = [
    ({
      lib,
      modulesPath,
      ...
    }: {
      imports = [(modulesPath + "/installer/scan/not-detected.nix")];
      networking.useDHCP = lib.mkDefault true;
      nixpkgs.hostPlatform = lib.mkDefault "x86_64-linux";
    })
  ];

  nix.desktop = [
    inputs.disko.nixosModules.default
    (import "${self}/assets/disko/ext4.nix" {device = "/dev/sda";})
    {
      fileSystems."/media/goat/BLUE_SATA" = {
        device = "/dev/disk/by-uuid/a6ffb4f9-049c-49a1-8b5f-1aca1b8dca08";
        fsType = "ext4";
      };

      boot = {
        initrd.availableKernelModules = ["nvme" "xhci_pci" "ahci" "usb_storage" "usbhid" "sd_mod"];
        initrd.kernelModules = [];
        extraModulePackages = [];
        kernelModules = ["kvm-amd"];
        supportedFilesystems = ["ntfs"];
      };
      networking.hostName = "goat-desktop";
      hardware.cpu.amd.updateMicrocode = true;
    }
  ];
  nix.macbook = [
    inputs.disko.nixosModules.default
    (import "${self}/assets/disko/ext4.nix" {device = "/dev/sda";})
    ({
      lib,
      config,
      pkgs,
      ...
    }: {
      nixpkgs.overlays = [(_: _: {minecraft-plymouth = inputs.minecraft-plymouth.defaultPackage.x86_64-linux;})];
      stylix.targets.plymouth.enable = false;

      boot = {
        initrd.availableKernelModules = ["xhci_pci" "ahci" "usbhid" "usb_storage" "sd_mod"];
        initrd.kernelModules = [];
        kernelModules = ["kvm-intel" "wl"];
        extraModulePackages = [config.boot.kernelPackages.broadcom_sta];
        blacklistedKernelModules = lib.mkForce ["b43" "bcma"];
        supportedFilesystems = ["ntfs" "btrfs" "apfs"];
        loader.grub.gfxmodeEfi = "2560x1600";

        plymouth = {
          enable = true;
          theme = "mc";
          themePackages = [pkgs.minecraft-plymouth];
        };

        # Enable "Silent boot"
        consoleLogLevel = 3;
        initrd.verbose = false;
        kernelParams = [
          "quiet"
          "splash"
          "boot.shell_on_fail"
          "udev.log_priority=3"
          "rd.systemd.show_status=auto"
        ];
        # Hide the OS choice for bootloaders.
        # It's still possible to open the bootloader list by pressing any key
        # It will just not appear on screen unless a key is pressed
        loader.timeout = 0;
      };
      networking.hostName = "goat-macbook";
      hardware.enableRedistributableFirmware = true;
    })
  ];
}
