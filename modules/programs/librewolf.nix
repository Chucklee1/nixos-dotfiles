{inputs, ...}: {
  linux.nix = [
    {nixpkgs.overlays = [inputs.nur.overlays.default];}
    {environment.variables.BROWSER = "librwolf";}
  ];
  linux.home = [
    ({pkgs, ...}: {
      stylix.targets.librewolf.profileNames = ["default"];
      programs.librewolf = {
        enable = true;
        settings = {
          "sidebar.verticalTabs" = true;
          "sidebar.expandOnHover" = true;
          "sidebar.main.tools" = "history,bookmarks";
          "browser.toolbars.bookmarks.visibility" = "never";
        };
        profiles.default = {
          name = "default";
          isDefault = true;
          search = {
            force = true;
            default = "StartPage";
            order = ["StartPage"];
            engines = {
              "Nix Packages" = {
                urls = [
                  {
                    template = "https://search.nixos.org/packages";
                    params = [
                      {
                        name = "type";
                        value = "packages";
                      }
                      {
                        name = "query";
                        value = "{searchTerms}";
                      }
                    ];
                  }
                ];
                icon = "''${pkgs.nixos-icons}/share/icons/hicolor/scalable/apps/nix-snowflake.svg";
                definedAliases = ["@np"];
              };
              "NixOS Wiki" = {
                urls = [{template = "https://nixos.wiki/index.php?search={searchTerms}";}];
                icon = "https://nixos.wiki/favicon.png";
                updateInterval = 24 * 60 * 60 * 1000; # every day
                definedAliases = ["@nw"];
              };
              "MyNixOS" = {
                urls = [{template = "https://mynixos.com/search?q={searchTerms}";}];
                icon = "https://mynixos.com/static/icons/mnos-logo.svg";
                updateInterval = 24 * 60 * 60 * 1000; # every day
                definedAliases = ["@mn"];
              };
              "StartPage".metaData.alias = "@sp"; # builtin engines only support specifying one additional alias
            };
          };
          extensions.packages = with pkgs.nur.repos.rycee.firefox-addons; [
            ublock-origin
            darkreader
            vimium
          ];
        };
      };
    })
  ];
}
