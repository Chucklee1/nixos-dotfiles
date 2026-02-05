{
  home = [
    ({
      config,
      pkgs,
      ...
    }: {
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
          plugin.append_fetchers = [
            {
              id = "git";
              name = "*";
              run = "git";
            }
            {
              id = "git";
              name = "*/";
              run = "git";
            }
          ];
          plugin.append_preloaders = [
            {
              mime = "{audio,video,image}/*";
              run = "mediainfo";
            }
            {
              mime = "application/subrip";
              run = "mediainfo";
            }
          ];
          plugin.append_previewers = [
            {
              mime = "{audio,video,image}/*";
              run = "mediainfo";
            }
            {
              mime = "application/subrip";
              run = "mediainfo";
            }
            {
              mime = "application/*zip";
              run = "ouch";
            }
            {
              mime = "application/x-tar";
              run = "ouch";
            }
            {
              mime = "application/x-bzip2";
              run = "ouch";
            }
            {
              mime = "application/x-7z-compressed";
              run = "ouch";
            }
            {
              mime = "application/x-rar";
              run = "ouch";
            }
            {
              mime = "application/vnd.rar";
              run = "ouch";
            }
            {
              mime = "application/x-xz";
              run = "ouch";
            }
            {
              mime = "application/xz";
              run = "ouch";
            }
            {
              mime = "application/x-zstd";
              run = "ouch";
            }
            {
              mime = "application/zstd";
              run = "ouch";
            }
            {
              mime = "application/java-archive";
              run = "ouch";
            }
          ];
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

        # stolen from: kerfuzzle's fork of stylix on github
        # file:        yazi-v26.1.22/modules/yazi/hm.nix
        # commit:      07f41f942ae752ab3b9f9071e45d30b37e4c0785
        theme = with config.lib.stylix.colors.withHashtag; {
          config.stylix.
          icon = let
            mkIcon = text: fg: {inherit text fg;};
          in {
            dirs = let
              mkDirIcon = name: text: fg: ((mkIcon text fg) // {inherit name;});
            in [
              (mkDirIcon ".config" "" orange)
              (mkDirIcon ".git" "" cyan)
              (mkDirIcon ".github" "" blue)
              (mkDirIcon ".npm" "" blue)
              (mkDirIcon "Desktop" "" cyan)
              (mkDirIcon "Development" "" cyan)
              (mkDirIcon "Documents" "" cyan)
              (mkDirIcon "Downloads" "" cyan)
              (mkDirIcon "Library" "" cyan)
              (mkDirIcon "Movies" "" cyan)
              (mkDirIcon "Music" "" cyan)
              (mkDirIcon "Pictures" "" cyan)
              (mkDirIcon "Public" "" cyan)
              (mkDirIcon "Videos" "" cyan)
            ];

            conds = let
              mkCondsIcon = cond: text: fg: ((mkIcon text fg) // {"if" = cond;});
            in [
              # Special files
              (mkCondsIcon "orphan" "" base05)
              (mkCondsIcon "link" "" base04)
              (mkCondsIcon "block" "" yellow)
              (mkCondsIcon "char" "" yellow)
              (mkCondsIcon "fifo" "" yellow)
              (mkCondsIcon "sock" "" yellow)
              (mkCondsIcon "sticky" "" yellow)
              (mkCondsIcon "dummy" "" red)

              # Fallback
              (mkCondsIcon "dir" "" blue)
              (mkCondsIcon "exec" "" green)
              (mkCondsIcon "!dir" "" base05)
            ];
          };
        };
      };
    })
  ];
}
