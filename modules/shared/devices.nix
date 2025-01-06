{
  pkgs,
  def,
  ...
}: {
  console = {
    earlySetup = true;
    keyMap = def.layout;
  };

  # xserver
  services.xserver.xkb.layout = def.layout;

  # audio
  services.pipewire = {
    enable = true;
    alsa.enable = true;
    alsa.support32Bit = true;
    pulse.enable = true;
  };

  # bluetooth
  hardware = {
    bluetooth.enable = true;
    bluetooth.powerOnBoot = true;
  };
  services.blueman.enable = true;

  # opengl - renamed to graphics as of 24.11
  hardware = {
    graphics.enable = true;
    graphics.enable32Bit = true;
  };

  # tablet support
  programs.weylus.enable = true;
  services.udev.extraRules = ''
    KERNEL=="uinput", MODE="0660", GROUP="uinput", OPTIONS+="static_node=uinput"
  '';
}
