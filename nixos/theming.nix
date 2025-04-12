{inputs, ...}: {
  nix.global = [
    inputs.stylix.nixosModules.stylix
    ({pkgs, ...}: {
      stylix = {
        # re-defined here since darwin does not have these options on system level
        cursor.package = pkgs.bibata-cursors;
        cursor.name = "Bibata-Modern-Classic";
        cursor.size = 24;

        fonts.sizes = {
          applications = 12;
          terminal = 12;
          desktop = 11;
          popups = 12;
        };
      };
    })
  ];
}
