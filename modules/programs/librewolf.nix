{inputs, ...}: {
  global.nix = [{nixpkgs.overlays = [inputs.nur.overlay];}];
  global.home = [
    ({pkgs, ...}: {
      librewolf = {
        settings = {
          # security
          "privacy.resistFingerprinting" = false;
          "security.OCSP.require" = false;
          # layout
          "sidebar.verticalTabs" = true;
          "sidebar.expandOnHover" = true;
          "sidebar.main.tools" = "history,bookmarks";
          "browser.toolbars.bookmarks.visibility" = "never";
        };
        default = {
          id = 0;
          name = "default";
          isDefault = true;
          settings = {
            "browser.startup.homepage" = "https://searx.aicampground.com";
            "browser.search.defaultenginename" = "";
            "browser.search.order.1" = "Searx";
          };
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
                iconUpdateURL = "https://nixos.wiki/favicon.png";
                updateInterval = 24 * 60 * 60 * 1000; # every day
                definedAliases = ["@nw"];
              };
              "MyNixOS" = {
                urls = [{template = "https://mynixos.com/search?q={searchTerms}";}];
                iconUpdateURL = "https://mynixos.com/static/icons/mnos-logo.svg";
                updateInterval = 24 * 60 * 60 * 1000; # every day
                definedAliases = ["@mn"];
              };
              "StartPage".metaData.alias = "@sp"; # builtin engines only support specifying one additional alias
            };
          };
          extensions = with pkgs.nur.repos.rycee.firefox-addons; [
            ublock-origin
            darkreader
            vimium
          ];
        };
      };
    })
  ];
}
