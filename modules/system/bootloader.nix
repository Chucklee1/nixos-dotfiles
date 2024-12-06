{...}: {
  boot.loader = {
    efi.efiSysMountPoint = "/boot/efi";
    grub = {
      enable = true;
      efiSupport = true;
      efiInstallAsRemovable = true;
      device = "nodev";
      boot.loader.grub.useOSProber = true;
    };
  };
}
