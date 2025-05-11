{
  inputs,
  self,
  ...
}: {
  nix.global = [
    ({lib, ...}: {
      hardware.enableRedistributableFirmware = lib.mkDefault true;
      nixpkgs.hostPlatform = "x86_64-linux";
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
        loader.grub.gfxmodeEfi = "1920x1080x30,auto";
      };
      hardware.cpu.amd.updateMicrocode = true;
    }
  ];
  nix.macbook = [
    inputs.disko.nixosModules.default
    (import "${self}/assets/disko/ext4.nix" {device = "/dev/sda";})
    ({
      lib,
      config,
      ...
    }: {
      boot = {
        initrd.availableKernelModules = ["xhci_pci" "ahci" "usbhid" "usb_storage" "sd_mod"];
        # wl and following 2 lines are a fix to broadcom 43 wifi drivers issues
        kernelModules = ["kvm-intel" "wl"];
        extraModulePackages = [config.boot.kernelPackages.broadcom_sta];
        blacklistedKernelModules = lib.mkForce ["b43" "bcma"];
        supportedFilesystems = ["ntfs" "btrfs" "apfs"];
        # settings for goofy 6:10 macbookpro-12-1 screen
        loader.grub.gfxmodeEfi = "2560x1600";
      };
    })
  ];
}
