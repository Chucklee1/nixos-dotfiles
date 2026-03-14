{
  home = [{
    programs.qutebrowser = {
      enable = true;
      searchEngines = {
        "@mn" = "https://mynixos.com/search?q={}";
        "@np" = "https://search.nixos.org/packages?channel=unstable&query=%{}";
        "@no" = "https://search.nixos.org/options?channel=unstable&query={}";
      };
    };
  }];
}
