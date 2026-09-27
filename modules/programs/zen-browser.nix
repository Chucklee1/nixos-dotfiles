{inputs, ...}: {
  nix = [
    {nixpkgs.overlays = [inputs.nur.overlays.default];}
  ];

  home = [
    inputs.zen-browser.homeModules.twilight
    ({
      lib,
      pkgs,
      ...
    }: let
      nixosIcons = "${pkgs.nixos-icons}/share/icons/hicolor/scalable/apps";
    in {
      stylix.targets.zen-browser.profileNames = ["default"];
      programs.zen-browser = {
        enable = true;
        setAsDefaultBrowser = true;
        enablePrivateDesktopEntry = true;
        languagePacks = ["en-US"];
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
          SanitizeOnShutdown = {
            FormData = true;
            Cache = true;
          };
        };
        profiles.default = {
          containersForce = true;
          pinsForce = true;
          spacesForce = true;
          extensions.packages = with pkgs.nur.repos.rycee.firefox-addons; [ublock-origin];
          settings = {
            "browser.aboutConfig.showWarning" = false;
            "privacy.userContext.enabled" = false; # disable containers
            "toolkit.legacyUserProfileCustomizations.stylesheets" = true;
            "zen.welcome-screen.seen" = true;
            "zen.urlbar.behavior" = "normal";
            "zen.view.compact.enable-at-startup" = true;
            "zen.view.compact.hide-toolbar" = true;
            "zen.view.compact.hide-tabbar" = true;
            "zen.view.sidebar-expanded" = false;
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
                      (lib.nameValuePair "channel" "unstable")
                      (lib.nameValuePair "query" "{searchTerms}")
                    ];
                  }
                ];
                icon = "${nixosIcons}/nix-snowflake.svg";
                definedAliases = ["@np"];
              };
              "MyNixOS" = {
                urls = [{template = "https://mynixos.com/search?q={searchTerms}";}];
                icon = "${nixosIcons}/nix-snowflake-white.svg";
                definedAliases = ["@mn"];
              };
            };
          };
          spaces = {
            "Default" = {
              id = "2bea09b3-85df-4cb9-9437-2ce7cd314341";
              icon = "chrome://browser/skin/zen-icons/selectable/cafe.svg";
              position = 1000;
            };
            "Nerd" = {
              id = "eb35ce6c-3084-4435-b0db-70358d7b4e53";
              icon = "chrome://browser/skin/zen-icons/selectable/code.svg";
              position = 2000;
            };
            "School" = {
              id = "3cb834a1-0629-431e-a0f7-28f9cc609713";
              icon = "chrome://browser/skin/zen-icons/selectable/school.svg";
              position = 3000;
            };
          };
        };
      };
    })
  ];
}
