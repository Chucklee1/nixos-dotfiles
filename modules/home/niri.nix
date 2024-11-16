{
  lib,
  config,
  ...
}: {
  programs.niri.settings = {
    binds = with config.lib.niri.actions; let
      sh = spawn "sh" "-c";
    in {
      "Mod+Return".action = spawn "kitty";
      "Mod+Space".action = spawn "fuzzel";
      "Mod+E".action = spawn "thunar";

      "Mod+Q".action = close-window;
      "Mod+Down".action = focus-column-left;
      "Mod+Up".action = focus-column-right;
    };
  };
}
