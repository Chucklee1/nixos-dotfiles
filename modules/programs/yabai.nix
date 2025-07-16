{
  nix.macbook = [
    ({config, ...}: {
      jankyborders = with config.lib.stylix.colors; {
        active_color = ''0xFF${base0D}'';
        inactive_color = ''0x00${base0D}'';
        style = "round";
        width = 1.0;
      };
    })
  ];
}
