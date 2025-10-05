{
  nix = [
    ({modulesPath, ...}: {
      imports = ["${modulesPath}/installer/cd-dvd/installation-cd-minimal.nix"];
      boot.zfs.forceImportRoot = false;
      isoImage = {
        showConfiguration = true;
        configurationName = "niri wayland (zsh + zfs)";
      };
      stylix.targets.grub.enable = false;
      services.displayManager = {
        cosmic-greeter.enable = true;
        autoLogin.enable = true;
        autoLogin.user = "nixos";
      };
    })
  ];
}
