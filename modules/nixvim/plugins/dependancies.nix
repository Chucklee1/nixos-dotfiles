{
  home.global = [
    ({pkgs, ...}: {
      programs.nixvim = {
        extraPlugins = with pkgs.vimPlugins; [
          plenary-nvim
        ];
        plugins.web-devicons.enable = true;
      };
    })
  ];
}
