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
  # services
  # -----------------------------------------------------------
  services = {
    pipewire = {
      enable = true;
      alsa.enable = true;
      alsa.support32Bit = true;
      pulse.enable = true;
    };
    displayManager.sddm = {
      enable = true;
      wayland.enable = true;
      extraPackages = [pkgs.sddm-astronaut];
      package = pkgs.kdePackages.sddm-kcm;
      theme = "sddm-astronaut-theme";
    };
    blueman.enable = true;
    printing.enable = true;
    fstrim.enable = true;
    openssh.enable = true;
  };
}
