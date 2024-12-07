{
  pkgs,
  config,
  lib,
  ...
}: {
  # -----------------------------------------------------------
  # hardware
  # -----------------------------------------------------------
  hardware = {
    graphics.enable = true; # renamed opengl to graphics as of 24.11
    graphics.enable32Bit = true;
    bluetooth.enable = true;
    bluetooth.powerOnBoot = true;
  };

  # -----------------------------------------------------------
  # hardware - gpu
  # -----------------------------------------------------------
  options = {
    radeon.enable = lib.mkEnableOption "enable radeon gpu drivers";
    nvidia.enable = lib.mkEnableOption "enable nvidia drivers";
  };

  config = {
    # gpu drivers for Xorg & Wayland
    services.xserver.videoDrivers =
      lib.optionals config.nvidia.enable ["nvidia"]
      ++ lib.optionals config.radeon.enable ["amd"];

    hardware.nvidia = lib.mkIf config.nvidia.enable {
      powerManagement.enable = false;
      powerManagement.finegrained = false;
      open = false;
      nvidiaSettings = true;
      package = config.boot.kernelPackages.nvidiaPackages.stable;
    };
  };

  # -----------------------------------------------------------
  # services
  # -----------------------------------------------------------
  services = {
    pipewire = {
      enable = true;
      alsa.enable = true;
      alsa.support32Bit = true;
      pulse.enable = true;
    };
    displayManager.ly.enable = true;
    gvfs.enable = true;
    tumbler.enable = true;
    blueman.enable = true;
    printing.enable = true;
    fstrim.enable = true;
    openssh.enable = true;
  };
}
