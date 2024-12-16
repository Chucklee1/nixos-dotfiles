{
  config,
  lib,
  pkgs,
  defaults,
  ...
}: {
  options.hyprland.enable = lib.mkEnableOption "enable hyprland window manager";

  config = lib.mkIf config.hyprland.enable {
    programs.hyprland.enable = true;

    home-manager.sharedModules = [
      {
        wayland.windowManager.hyprland = {
          enable = true;
          plugins = [pkgs.hyprlandPlugins.hyprscroller];
        };
      }
    ];
  };
}
