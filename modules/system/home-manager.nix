{inputs, ...}: {
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
