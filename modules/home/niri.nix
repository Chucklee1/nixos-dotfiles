{
  lib,
  config,
  ...
}: {
  programs.niri.settings = {
    prefer-no-csd = true;
    hotkey-overlay.skip-at-startup = false;
    screenshot-path = "~/Pictures/Screenshots/Screenshot from %Y-%m-%d %H-%M-%S.png";

    binds = with config.lib.niri.actions; let
      sh = spawn "sh" "-c";
    in {
      "Mod+Return".action = spawn "kitty";
      "Mod+Space".action = spawn "fuzzel";
      "Mod+E".action = spawn "thunar";

      "Mod+Q".action = close-window;
      "Mod+Left".action = focus-column-left;
      "Mod+Right".action = focus-column-right;
    };
  };
}
