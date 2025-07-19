let
  nixvirt = [
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
  homevirt = [
    {
      dconf.settings."org/virt-manager/virt-manager/connections" = {
        autoconnect = ["qemu:///system"];
        uris = ["qemu:///system"];
      };
    }
  ];
in {
  nix.desktop = nixvirt;

  nix.umbra = [
    ({
      pkgs,
      modulesPath,
      user,
      ...
    }: {
      imports = ["${modulesPath}/virtualisation/qemu-vm.nix"];
      virtualisation.qemu.options = ["-device virtio-vga"];
      virtualisation.vmVariant = {
        virtualisation.memorySize = 8192;
        virtualisation.cores = 6;
      };

      boot.loader.systemd-boot.enable = true;
      boot.loader.efi.canTouchEfiVariables = true;

      services.xserver.enable = true;

      users.users.${user} = {
        isNormalUser = true;
        extraGroups = ["wheel"];
        initialPassword = "password";
      };

      environment.systemPackages = with pkgs; [git neovim];

      system.stateVersion = "24.05";
    })
  ];

  home.desktop = homevirt;
}
