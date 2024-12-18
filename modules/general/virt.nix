{
  config,
  lib,
  pkgs,
  ...
}: {
  options.virt.enable = lib.mkEnableOption "enables vm utils";

  config = lib.mkIf config.virt.enable {
    programs = {
      virt-manager.enable = true;
      dconf.enable = true;
    };
    virtualisation = {
      spiceUSBRedirection.enable = true;
      libvirtd = {
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
    home-manager.sharedModules = [
      {
        dconf.settings = {
          "org/virt-manager/virt-manager/connections" = {
            autoconnect = ["qemu:///system"];
            uris = ["qemu:///system"];
          };
        };
      }
    ];
  };
}
