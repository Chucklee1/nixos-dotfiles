{
  home = [{
    home.sessionVariables.BROWSER = "qutebrowser";
    programs.qutebrowser = {
      enable = true;
      searchEngines = {
        "@mn" = "https://mynixos.com/search?q={}";
        "@np" = "https://search.nixos.org/packages?channel=unstable&query=%{}";
        "@no" = "https://search.nixos.org/options?channel=unstable&query={}";
      };
    };
    xdg.mimeApps = {
      enable = true;
      defaultApplications = let
        browser = "org.qutebrowser.qutebrowser.desktop";
      in {
        "text/html" = browser;
        "x-scheme-handler/http" = browser;
        "x-scheme-handler/https" = browser;
        "x-scheme-handler/about" = browser;
        "x-scheme-handler/unknown" = browser;
      };
    };
  }];
}
