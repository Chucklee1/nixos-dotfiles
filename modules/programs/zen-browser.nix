{inputs, ...}: {
  nix = [
    {nixpkgs.overlays = [inputs.nur.overlays.default];}
  ];

  home = [
    inputs.zen-browser.homeModules.twilight
    ({pkgs, ...}: {
      stylix.targets.zen-browser.profileNames = ["default"];
      home.sessionVariables.BROWSER = "zen-twilight";
      programs.zen-browser = {
        enable = true;
        policies = {
          AutofillAddressEnabled = true;
          AutofillCreditCardEnabled = false;
          DisableAppUpdate = true;
          DisableFeedbackCommands = true;
          DisableFirefoxStudies = true;
          DisablePocket = true;
          DisableTelemetry = true;
          DontCheckDefaultBrowser = true;
          NoDefaultBookmarks = true;
          OfferToSaveLogins = false;
          EnableTrackingProtection = {
            Value = true;
            Locked = true;
            Cryptomining = true;
            Fingerprinting = true;
          };
          profiles.default = {
            name = "default";
            isDefault = true;
            settings = {
              "zen.view.compact.hide-tabbar" = true;
              "zen.view.compact.hide-toolbar" = true;
              "zen.welcome-screen.seen" = true;
              "zen.urlbar.behavior" = "normal";
            };
            search = {
              force = true;
              default = "ddg";
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
              };
            };
            extensions.packages = with pkgs.nur.repos.rycee.firefox-addons; [ublock-origin];
          };
        };
      };
      xdg.mimeApps = {
        enable = true;
        defaultApplications = let
          browser = "zen-twilight.desktop";
        in {
          "text/html" = browser;
          "x-scheme-handler/http" = browser;
          "x-scheme-handler/https" = browser;
          "x-scheme-handler/about" = browser;
          "x-scheme-handler/unknown" = browser;
        };
      };
    })
  ];
}
