{pkgs, ...}: {
  home-manager.sharedModules = [
    {
      programs.nixvim = {
        plugins = {
          lazygit.enable = true;
          web-devicons.enable = true;
	  nvim-tree.enable = true;
        };
      };
    }
  ];
}
