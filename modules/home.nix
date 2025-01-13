{
  home.global = {
    inputs,
    def,
    ...
  }: {
    home-manager = {
      useUserPackages = true;
      useGlobalPkgs = true;
      extraSpecialArgs = {inherit inputs def;};
      users.${def.username}.home = {
        stateVersion = "24.05"; # DO NOT CHANGE
        username = "${def.username}";
        homeDirectory = "/home/${def.username}";
      };
    };
  };
}
