{
  config,
  lib,
  pkgs,
  ...
}: {
  options.thunar.enable = lib.mkEnableOption "enable thunar file manager";

  config = lib.mkIf config.thunar.enable {
    programs.thunar = {
      enable = true;
      plugins = with pkgs.xfce; [
        thunar-archive-plugin
        thunar-volman
        thunar-media-tags-plugin
      ];
    };
    services = {
      gvfs.enable = true;
      tumbler.enable = true;
    };
  };
}
