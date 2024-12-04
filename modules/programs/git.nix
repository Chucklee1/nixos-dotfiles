{pkgs, ...}: {
  home-manager.sharedModules.programs = {
    lazygit.enable = true;
    git = {
      enable = true;
      userEmail = "cooperkang4@gamil.com";
      userName = "Chucklee1";
    };
  };
}
