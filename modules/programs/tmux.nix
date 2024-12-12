{...}: {
  home-manager.sharedModules = [
    {
      programs.tmux = {
        enable = true;
      };
    }
  ];
}