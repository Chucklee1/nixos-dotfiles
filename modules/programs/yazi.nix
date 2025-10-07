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
          bookmarks = pkgs.fetchFromGitHub {
            owner = "dedukun";
            repo = "bookmarks.yazi";
            rev = "9ef1254d8afe88aba21cd56a186f4485dd532ab8";
            hash = "sha256-GQFBRB2aQqmmuKZ0BpcCAC4r0JFKqIANZNhUC98SlwY=";
          };
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
            require("bookmarks"):setup({
              last_directory = { enable = false, persist = false, mode="dir" },
              persist = "none",
              desc_format = "full",
              file_pick_mode = "hover",
              custom_desc_input = false,
              show_keys = false,
              notify = {
                enable = false,
                timeout = 1,
                message = {
                  new = "New bookmark '<key>' -> '<folder>'",
                  delete = "Deleted bookmark in '<key>'",
                  delete_all = "Deleted all bookmarks",
                },
              },
            })
          '';

        settings = {
          mgr.show_hidden = true;
          preview = {
            max_width = 1000;
            max_height = 1000;
          };
          plugin.append_fetchers = [
            {id = "git"; name = "*"; run = "git";}
            {id = "git"; name = "*/"; run = "git";}
          ];
          plugin.append_preloaders = [
            {mime = "{audio,video,image}/*"; run = "mediainfo";}
            {mime = "application/subrip"; run = "mediainfo";}
          ];
          plugin.append_previewers = [
            {mime = "{audio,video,image}/*"; run = "mediainfo";}
            {mime = "application/subrip"; run = "mediainfo";}
            {mime = "application/*zip"; run = "ouch";}
            {mime = "application/x-tar"; run = "ouch";}
            {mime = "application/x-bzip2"; run = "ouch";}
            {mime = "application/x-7z-compressed"; run = "ouch";}
            {mime = "application/x-rar"; run = "ouch";}
            {mime = "application/vnd.rar"; run = "ouch";}
            {mime = "application/x-xz"; run = "ouch";}
            {mime = "application/xz"; run = "ouch";}
            {mime = "application/x-zstd"; run = "ouch";}
            {mime = "application/zstd"; run = "ouch";}
            {mime = "application/java-archive"; run = "ouch";}
          ];
        };

        keymap = {
          mgr.append_keymap = [
            # chmod
            {on = ["c" "m"]; run = "plugin chmod";}
            # mount
            {on = ["M"]; run = "plugin mount";}
            # bookmarks
            {on = ["b" "m"]; run = "plugin bookmarks save";}
            {on = ["b" "j"]; run = "plugin bookmarks jump";}
            {on = ["b" "d"]; run = "plugin bookmarks delete";}
            {on = ["b" "D"]; run = "plugin bookmarks delete_all";}
            # restore
            {on = "u"; run = "plugin restore";}
            {on = ["d" "u"]; run = "plugin restore";}
            {on = ["d" "U"]; run = "shell --block -- clear && trash-restore --overwrite";}
          ];
        };
      };
    })
  ];
}
