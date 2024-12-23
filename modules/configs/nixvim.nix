{pkgs, ...}: {
  home-manager.sharedModules = [
    {
      programs.nixvim = {
        enable = true;
      };
    }
  ];
}
