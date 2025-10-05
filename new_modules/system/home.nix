{inputs, extlib, ...}: {
  nix = [
    (extlib.darwinOrLinux
      inputs.home-manager.darwinModules.home-manager
      inputs.home-manager.nixosModules.home-manager
    )
    ({config, user, ...}: {
      home-manager.useGlobalPkgs = true;
      home-manager.backupFileExtension = "backup";
      home-manager.users.${user} = {
        home = {
          stateVersion = "24.05"; # DO NOT CHANGE
          username = user;
          homeDirectory = config.users.users.${user}.home;
        };
        imports = config._module.args.homeMods;
      };
    })
  ];
}
