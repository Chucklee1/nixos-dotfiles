{...}: {
  users.users.goat = {
    isNormalUser = true;
    extraGroups = ["wheel" "networkmanager"];
  };
  home-manager.users.goat = {
    home.stateVersion = "24.05"; # DO NOT CHANGE
    home.username = "goat";
    home.homeDirectory = "/home/goat";
  };
}
