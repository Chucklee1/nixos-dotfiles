{
  nix = [
    {
      hardware.uinput.enable = true;
      programs.weylus.enable = true;
      services.udev.extraRules = ''KERNEL=="uinput", MODE="0660", GROUP="uinput", OPTIONS+="static_node=uinput" '';
    }
  ];
}
