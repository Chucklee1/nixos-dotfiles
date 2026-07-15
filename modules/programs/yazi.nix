{
  home = [
    ({pkgs, ...}: {
      programs.yazi = {
        enable = true;
        shellWrapperName = "y";

        plugins = {
          chmod = pkgs.yaziPlugins.chmod;
          git = pkgs.yaziPlugins.git;
          mount = pkgs.yaziPlugins.mount;
          ouch = pkgs.yaziPlugins.ouch;
          restore = pkgs.yaziPlugins.restore;
          exifaudio = pkgs.fetchFromGitHub {
            owner = "Sonico98";
            repo = "exifaudio.yazi";
            rev = "4506f9d5032e714c0689be09d566dd877b9d464e";
            hash = "sha256-RWCqWBpbmU3sh/A+LBJPXL/AY292blKb/zZXGvIA5/o=";
          };
        };

        initLua =
          #lua
          ''
            require("git"):setup()
          '';

        settings = {
          mgr.show_hidden = true;
          preview = {
            max_width = 1000;
            max_height = 1000;
          };
        };

        keymap = {
          mgr.append_keymap = [
            # chmod
            {
              on = ["c" "m"];
              run = "plugin chmod";
            }
            # mount
            {
              on = ["M"];
              run = "plugin mount";
            }
            # bookmarks
            {
              on = ["b" "m"];
              run = "plugin bookmarks save";
            }
            {
              on = ["b" "j"];
              run = "plugin bookmarks jump";
            }
            {
              on = ["b" "d"];
              run = "plugin bookmarks delete";
            }
            {
              on = ["b" "D"];
              run = "plugin bookmarks delete_all";
            }
            # restore
            {
              on = "u";
              run = "plugin restore";
            }
            {
              on = ["d" "u"];
              run = "plugin restore";
            }
            {
              on = ["d" "U"];
              run = "shell --block -- clear && trash-restore --overwrite";
            }
          ];
        };
      };
    })
  ];
}
