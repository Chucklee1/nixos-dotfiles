{pkgs, ...}: {
  home-manager.users.goat.programs.neovim = {
    enable = true;
    defaultEditor = true;
    viAlias = true;
    vimAlias = true;
    extraConfig = ''
      set clipboard=unnamedplus
      set number
      set tabstop=2
      set shiftwidth=2
      set expandtab  " Use spaces instead of tabs
    '';
  };
}
