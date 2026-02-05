{
  inputs,
  mod,
  ...
}:
with mod; {
  system = "x86_64-linux";
  builder = inputs.nixpkgs.lib.nixosSystem;
  user = "nixos";
  modules = with mod; [
    system.boot
    system.home
    system.pkgconfig
    system.sys-specs
    drivers.ssh
    shell.zsh
    programs.nixvim
  ];

  extraConfig = [
    ({modulesPath, ...}: {
      imports = ["${modulesPath}/installer/cd-dvd/installation-cd-minimal.nix"];
      boot.zfs.forceImportRoot = false;
      isoImage = {
        showConfiguration = true;
        configurationName = "custom minimal installer";
      };
    })
  ];
}
