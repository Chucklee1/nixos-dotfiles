{
  nix = [
    ({
      pkgs,
      user,
      ...
    }: {
      users.users.${user} = {
        extraGroups = ["libvirtd" "kvm"];
      };

      # networking.firewall.trustedInterfaces = [ "virbr0" ];
      environment.systemPackages = [
        pkgs.swtpm
      ];

      programs.virt-manager.enable = true;
      virtualisation = {
        libvirtd.enable = true;
        libvirtd.qemu.swtpm.enable = true;
        spiceUSBRedirection.enable = true;
      };
    })
  ];
}
