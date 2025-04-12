{
  inputs,
  username,
  ...
}: {
  home-manager = {
    extraSpecialArgs = {inherit inputs username;};
    useGlobalPkgs = true;
    useUserPackages = true;
    users.${username} = {
      imports = [
        ./nixvim.nix
        ./software.nix
      ];
      home.stateVersion = "24.05";
    };
  };
}
