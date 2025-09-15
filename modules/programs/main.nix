{self, ...}: {
  global.home = [
    {
      xdg.desktopEntries = {
        emacs = {
          name = "Emacs";
          genericName = "OS in a Text Editor";
          exec = "emacs %F";
          terminal = true;
          categories = ["Application" "TextEditor"];
          mimeType = ["text/org" "text/el" "text/tex" "text/sty"];
        };
      };
    }
    {
      programs = {
        git = {
          enable = true;
          userEmail = "kermitthefrog@kakao.com";
          userName = "Chucklee1";
        };
        lazygit = {
          enable = true;
          settings.notARepository = "skip";
          settings.promptToReturnFromSubprocess = false;
        };
        kitty = {
          enable = true;
          settings = {
            cursor_shape = "beam";
            macos_hide_titlebar = "yes";
            background_blur = 40;
            confirm_os_window_close = 0;
            tab_bar_edge = "bottom";
            tab_bar_style = "powerline";
            tab_powerline_style = "round";
          };
        };
        btop.enable = true;
        direnv.enable = true;
        fzf.enable = true;
        zoxide = {
          enable = true;
          options = ["--cmd cd"];
        };
      };
    }
  ];

  additions.full.home = [
    ({pkgs, ...}: {
      programs.zathura.enable = true;
      home.packages = [pkgs.rmpc];
      home.file.".config/rmpc".source = "${self}/assets/rmpc";
    })
  ];
}
