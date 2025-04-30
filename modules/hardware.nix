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
    inputs.nixos-hardware.nixosModules.apple-macbook-pro-12-1
    inputs.disko.nixosModules.default
    (import "${self}/assets/disko/btrfs.nix" {device = "/dev/sda";})
    {
      boot = {
        initrd.availableKernelModules = ["xhci_pci" "ahci" "usbhid" "usb_storage" "sd_mod"];
        initrd.kernelModules = [];
        kernelModules = ["kvm-intel"];
        extraModulePackages = [];
        supportedFilesystems = ["ntfs" "btrfs" "apfs"];
      };
      networking.hostName = "goat-macbook";
      hardware.enableRedistributableFirmware = true;
    }
  ];
}
