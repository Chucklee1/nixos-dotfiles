{
  pkgs,
  def,
  inputs,
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
    sharedModules = [
      ./shelli.home.nix
      ./nixvim.home.nix
      {
        # software
        home.packages = with pkgs; [
          krita
          webcord
          spotify
          zoom-us
        ];
 
        programs.firefox.enable = true;
        services.gnome-keyring.enable = true;
        stylix.targets.waybar.enable = false;
        
        # user theming
        gtk = {
          enable = true;
          iconTheme.name = "Papirus-Dark";
          iconTheme.package = pkgs.papirus-icon-theme;
          gtk3.extraConfig.gtk-application-prefer-dark-theme = 1;
          gtk4.extraConfig.gtk-application-prefer-dark-theme = 1;
        };
        qt = {
          enable = true;
          style.name = "adwaita-dark";
          platformTheme.name = "gtk3";
        };
      }
    ];
  };
}
