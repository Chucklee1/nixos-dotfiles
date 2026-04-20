{
  home = [{
    programs.helix = {
      enable = true;
      defaultEditor = true;
      settings = {
        editor = {
          clipboard-provider = "wayland";
          default-yank-register = "+";
          cursor-shape = {
            insert = "bar";
            normal = "block";
            select = "underline";
          };
          smart-tab.enable = true;
        };
        # fake emacs aah
        keys.normal."C-x" = {
          "k" = ":bc";
          "C-;" = "toggle_comments";
          "C-c" = ":q";
          "C-j" = "file_explorer";
          "C-s" = ":w";
        };
      };
    };
  }];
}
