{
  pkgs,
  lib,
  config,
  ...
}: {
  home-manager.sharedModules = [
    {
      home.packages = with pkgs; [
        vulkan-tools
        mangohud
      ];
    }
  ];
}
