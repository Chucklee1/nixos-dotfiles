{
  nix.macbook = [
    ({config, ...}: {
      jankyborders = with config.lib.stylix.colors; {
        enable = true;
        active_color = ''0xFF${base0D}'';
        inactive_color = ''0x00${base0D}'';
        style = "round";
        width = 1.0;
      };
    })
  ];
}
