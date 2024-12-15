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
  security.rtkit.enable = true; # sound thing

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
    xserver.enable = true;
    xserver.displayManager.lightdm.greeters.slick.enable = true;
    blueman.enable = true;
    printing.enable = true;
    fstrim.enable = true;
    openssh.enable = true;
  };
}
