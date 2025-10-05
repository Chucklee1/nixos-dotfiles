{inputs, ...}: {
  nix = [
    {nixpkgs.overlays = [inputs.nur.overlays.default];}
    {environment.variables.BROWSER = "librwolf";}
  ];

  home = [
    ({pkgs, ...}: {
      stylix.targets.librewolf.profileNames = ["default"];
      programs.librewolf = {
        enable = true;
        settings = {
          # allow css customization
          "toolkit.legacyUserProfileCustomizations.stylesheets" = true;
          # sidebar
          "sidebar.new-sidebar.has-used" = true;
          "sidebar.revampt" = true;
          "sidebar.visibility" = "expand-on-hover";
          "sidebar.main.tools" = "history,bookmarks";
          # security
          # Would of kept defaults but half the sites don't work cause of it
          "dom.security.https_only_mode_ever_enabled" = true;
          "security.OCSP.require" = false;
          "security.tls.enable_0rtt_data" = false;
          # toolbar
          "browser.toolbars.bookmarks.visibility" = "never";
          "browser.uiCustomization.state" = ''
            {
              "placements": {
                "widget-overflow-fixed-list": [],
                "unified-extensions-area": [],
                "nav-bar": [
                  "sidebar-button",
                  "unified-extensions-button",
                  "back-button",
                  "forward-button",
                  "stop-reload-button",
                  "urlbar-container",
                  "history-panelmenu",
                  "downloads-button",
                  "vertical-spacer"
                ],
                "toolbar-menubar": ["menubar-items"],
                "TabsToolbar": [],
                "vertical-tabs": ["tabbrowser-tabs"],
                "PersonalToolbar": ["personal-bookmarks"]
              },
              "seen": ["developer-button","screenshot-button"],
              "dirtyAreaCache": [
                "nav-bar",
                "TabsToolbar",
                "vertical-tabs",
                "toolbar-menubar",
                "PersonalToolbar"
              ],
            }
          '';
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
          extensions.packages = with pkgs.nur.repos.rycee.firefox-addons; [ublock-origin];
        };
      };
    })
  ];
}
