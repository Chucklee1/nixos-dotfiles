{
  lib,
  config,
  ...
}: {
  programs.niri.settings = {
    # general
    prefer-no-csd = true;
    hotkey-overlay.skip-at-startup = false;
    screenshot-path = "~/Pictures/Screenshots/Screenshot from %Y-%m-%d %H-%M-%S.png";
    # startup
    # inputs
    input.keyboard.xkb.layout = "us";
    input.mouse.accel-speed = 1.0;
    input.touchpad = {
      tap = true;
      dwt = true;
      natural-scroll = true;
      click-method = "clickfinger";
    };

    input.tablet.map-to-output = "eDP-1";
    input.touch.map-to-output = "eDP-1";

    # input.warp-mouse-to-focus = true;

    layout = {
      gaps = 8;
      border.width = 2;
      always-center-single-column = false;
    };
    # monitors
    outputs."DP-1" = {
      enable = true;
      mode.width = 1920;
      mode.height = 1080;
      position.x = 0;
      position.y = 0;
      mode.refresh = 165.001;
    };
    binds = with config.lib.niri.actions; let
      sh = spawn "sh" "-c";
    in {
      "Mod+Return".action = spawn "kitty";
      "Mod+Space".action = spawn "fuzzel";
      "Mod+E".action = spawn "thunar";

      "Mod+Q".action = close-window;
      "Mod+Left".action = focus-column-left;
      "Mod+Right".action = focus-column-right;
      "Mod+Up".action = focus-workspace-up;
      "Mod+Down".action = focus-workspace-down;
    };
  };
}
