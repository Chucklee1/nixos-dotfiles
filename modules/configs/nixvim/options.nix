{self, ...}: {
  home-manager.sharedModules = [
    {
      programs.nixvim = {
        globalOpts = {
          # Line numbers
          number = true;
        };
      };
    }
  ];
}
