{
  imports = [
    ./dwm.nix
    ./system.nix
    ./virt.nix
    ./hardware.nix
    ./nixvim.nix
    ./theming.nix
  ];
  /*
  defining main home setttings in import to ensure home-manager
  settings work. Propably going to move it to standalone soon
  */
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
