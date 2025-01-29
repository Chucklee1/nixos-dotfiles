{
  lib,
  config,
  pkgs,
  ...
}: let
  conf = opt: lib.mkIf config.${opt}.enable;
  opt = lib.mkOption {
    type = lib.types.bool;
    default = false;
    description = "read";
  };
in {
  _module.args.mk = {
    conf = opt: lib.mkIf config.${opt}.enable;
    opt = lib.mkOption {
      type = lib.types.bool;
      default = false;
      description = "read";
    };
  };

  lib.options = {
    laptop.enable = opt;
    desktop.enable = opt;
  };
  config = lib.mkMerge [
    # machines
    (conf "laptop" {
      radeon.enable = true;
      niri.enable = true; # also enables wayland
      steam.enable = true; # also enables wine
    })

    (conf "desktop" {
      # external options
      nvidia.enable = true;
      virt.enable = true;
      windowsCompat = true;
      niri.enable = true; # also enables wayland
      steam.enable = true; # also enables wine
      weylus.enable = true;

      # other drives
      fileSystems."/media/goat/BLUE_SATA" = {
        device = "/dev/disk/by-uuid/a6ffb4f9-049c-49a1-8b5f-1aca1b8dca08";
        fsType = "ext4";
      };
      networking.interfaces.enp7s0.useDHCP = lib.mkDefault true;
      networking.interfaces.wlp6s0.useDHCP = lib.mkDefault true;

      # extra games
      environment.systemPackages = with pkgs; [
        prismlauncher
        osu-lazer-bin
      ];
    })
  ];
}
