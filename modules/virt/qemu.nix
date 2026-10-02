{
  nix = [
    ({
      pkgs,
      user,
      ...
    }: {
      environment.systemPackages = [
        pkgs.guestfs-tools
        pkgs.virtiofsd
      ];

      users.users.${user} = {
        extraGroups = ["libvirtd" "kvm"];
      };

      programs.virt-manager.enable = true;
      virtualisation = {
        libvirtd.enable = true;
        libvirtd.qemu.swtpm.enable = true;
        spiceUSBRedirection.enable = true;
      };
    })
  ];
}
