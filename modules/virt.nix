{
  nix.global = [
    ({pkgs, ...}: {
      virtualisation = {
        containers.enable = true;
        podman = {
          enable = true;
          dockerCompat = true;
          defaultNetwork.settings.dns_enabled = true;
        };
      };

      # Useful other development tools
      environment.systemPackages = with pkgs; [
        dive # look into docker image layers
        podman-tui # status of containers in the terminal
        #docker-compose # start group of containers for dev
        podman-compose # start group of containers for dev
      ];
    })
  ];
  nix.desktop = [
    # virtualisation
    ({pkgs, ...}: {
      programs.virt-manager.enable = true;
      virtualisation = {
        spiceUSBRedirection.enable = true;
        libvirtd = {
          onBoot = "ignore";
          onShutdown = "shutdown";
          enable = true;
          qemu = {
            package = pkgs.qemu_kvm;
            runAsRoot = true;
            swtpm.enable = true;
            ovmf = {
              enable = true;
              packages = [
                (pkgs.OVMF.override {
                  secureBoot = true;
                  tpmSupport = true;
                })
                .fd
              ];
            };
          };
        };
      };
    })
  ];

  home.desktop = [
    {
      dconf.settings."org/virt-manager/virt-manager/connections" = {
        autoconnect = ["qemu:///system"];
        uris = ["qemu:///system"];
      };
    }
  ];
}
