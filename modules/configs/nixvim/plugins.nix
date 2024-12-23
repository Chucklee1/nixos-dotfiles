{pkgs, ...}: {
  home-manager.sharedModules = [
    {
      programs.nixvim = {
        plugins.lazygit.enable = true;
      };
    }
  ];
}
