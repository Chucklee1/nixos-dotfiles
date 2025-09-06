{
  virt.containers.nix = [
    # docker
    ({pkgs, ...}: {
      virtualisation.containers.enable = true;
      virtualisation = {
        podman = {
          enable = true;
          dockerCompat = true;
          defaultNetwork.settings.dns_enabled = true;
        };
      };

      environment.systemPackages = with pkgs; [
        dive # look into docker image layers
        podman-tui # status of containers in the terminal
        docker-compose # start group of containers for dev
        #podman-compose # start group of containers for dev
      ];
    })
  ];
  virt.qemu.nix = [
    ({pkgs, ...}: {
      users.groups.libvirtd.members = ["goat"];
      users.groups.kvm.members = ["goat"];
      programs.virt-manager.enable = true;
      virtualisation = {
        spiceUSBRedirection.enable = true;
        libvirtd = {
          enable = true;
          onBoot = "ignore";
          qemu = {
            package = pkgs.qemu_kvm;
            runAsRoot = true;
            swtpm.enable = true;
            ovmf = {
              enable = true;
              packages = [(pkgs.OVMF.override {
                secureBoot = true;
                tpmSupport = true;
              }).fd];
            };
          };
        };
      };
    })
  ];
  virt.qemu.home = [
    {
      dconf.settings."org/virt-manager/virt-manager/connections" = {
        autoconnect = ["qemu:///system"];
        uris = ["qemu:///system"];
      };
    }
  ];
}
