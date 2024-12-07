{inputs, ...}: {
  # -----------------------------------------------------------
  # system user declaration
  # -----------------------------------------------------------
  users.users.goat = {
    isNormalUser = true;
    extraGroups = ["wheel" "networkmanager"];
  };

  # -----------------------------------------------------------
  # home manager
  # -----------------------------------------------------------
  home-manager = {
    useUserPackages = true;
    useGlobalPkgs = true;
    extraSpecialArgs = {inherit inputs;};
    users.goat = {
      home.stateVersion = "24.05"; # DO NOT CHANGE
      home.username = "goat";
      home.homeDirectory = "/home/goat";
    };
  };
}
