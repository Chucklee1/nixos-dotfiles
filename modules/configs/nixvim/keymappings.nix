{self, ...}: {
  home-manager.sharedModules = [
    {
      programs.nixvim = {
        keymaps = [];
      };
    }
  ];
}
