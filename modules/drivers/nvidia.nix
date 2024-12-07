{
  config,
  lib,
  pkgs,
  ...
}: {
  options = {
    nvidia.enable = lib.mkEnableOption "enable nvidia drivers";
  };

  config = lib.mkIf config.nvidia.enable {
    # nvidia driver for Xorg & Wayland
    services.xserver.videoDrivers = ["nvidia"];
    hardware.nvidia = {
      powerManagement.enable = false;
      powerManagement.finegrained = false;
      open = false;
      nvidiaSettings = true;
      package = config.boot.kernelPackages.nvidiaPackages.stable;
    };
  };
}
