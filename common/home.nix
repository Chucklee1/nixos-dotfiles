{
  inputs,
  ops,
  ...
}: {
  home-manager = {
    extraSpecialArgs = {inherit inputs ops;};
    useGlobalPkgs = true;
    useUserPackages = true;
    users.${ops.user} = {
      imports = [
        ./home/bash.nix
        ./home/git.nix
        ./home/kitty.nix
        ./home/nixvim.nix
      ];
      home.stateVersion = "24.05";
    };
  };
}
