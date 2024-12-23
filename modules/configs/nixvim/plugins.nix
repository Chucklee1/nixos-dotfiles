{pkgs, ...}: {
  home-manager.sharedModules = [
    {
      programs.nixvim = {
        plugins = {
	  web-devicons.enable = true;
          lazygit.enable = true;
	  nvim-tree.enable = true;
	  bufferline.enable = true;
        };
      };
    }
  ];
}
