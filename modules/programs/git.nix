{pkgs, ...}: {
  home-manager.users.goat.programs = {
    lazygit.enable = true;
    git = {
      enable = true;
      userEmail = "cooperkang4@gamil.com";
      userName = "Chucklee1";
    };
  };
}
