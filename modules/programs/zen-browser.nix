{inputs, ...}: {
    nix = [
    {nixpkgs.overlays = [inputs.nur.overlays.default];}
    # {environment.variables.BROWSER = "";}
  ];

  home = [
    inputs.zen-browser.homeModules.twilight
    ({pkgs, ...}: {
      stylix.targets.librewolf.profileNames = ["default"];
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
          search = {
            force = true;
            default = "ddg";
            engines = {
              "Nix Packages" = {
                urls = [
                  {
                    template = "https://search.nixos.org/packages";
                    params = [
                      {name = "type"; value = "packages";}
                      {name = "query"; value = "{searchTerms}";}
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
    })
    # from repo's readme
    ({pkgs, ...}: {
      xdg.mimeApps = let
        value = let
          zen-browser = inputs.zen-browser.packages.${pkgs.stdenv.system}.twilight; # or twilight
        in
          zen-browser.meta.desktopFileName;

        associations = builtins.listToAttrs (map (name: {
          inherit name value;
        }) [
          "application/x-extension-shtml"
          "application/x-extension-xhtml"
          "application/x-extension-html"
          "application/x-extension-xht"
          "application/x-extension-htm"
          "x-scheme-handler/unknown"
          "x-scheme-handler/mailto"
          "x-scheme-handler/chrome"
          "x-scheme-handler/about"
          "x-scheme-handler/https"
          "x-scheme-handler/http"
          "application/xhtml+xml"
          "application/json"
          "text/plain"
          "text/html"
        ]);
      in {
        associations.added = associations;
        defaultApplications = associations;
      };
    })
  ];
}
