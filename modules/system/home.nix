{
  inputs,
  extlib,
  ...
}: {
  nix = [
    (extlib.darwinOrLinux
    inputs.home-manager.darwinModules.home-manager
    inputs.home-manager.nixosModules.home-manager)
    {
      home-manager.useGlobalPkgs = true;
      home-manager.backupFileExtension = "backup";
    }
  ];

  home = [
    ({user, ...}: {
      home = {
        stateVersion = "26.05";
        username = user;
        homeDirectory =
          extlib.darwinOrLinux
          "/Users/${user}" "/home/${user}";
      };
    })
  ];
}
