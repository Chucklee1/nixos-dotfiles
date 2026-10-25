{extlib, ...}: {
  nix = [
    ({pkgs, ...}: {
      environment.systemPackages = with pkgs;
        [
          # build stuff
          gcc
          just
          lua
          python3
          ghc
          # lsps
          asm-lsp # GAS/GO assembly
          clang-tools # c/c++
          cmake-language-server
          bash-language-server
          lua-language-server
          haskell-language-server
          marksman
          nixd
          python3Packages.python-lsp-server
          typescript-language-server
          vscode-langservers-extracted
          # diagnostics
          statix
          # formatters
          jq # it counts
          yq # also counts
          alejandra
          html-tidy
          prettier
          shfmt
          # info helpers
          ripgrep
          fzf
          fd
          pciutils
          # shells
          bash
          nushell
          powershell
          # media
          mediainfo
          ffmpegthumbnailer
          poppler
          ueberzugpp
          imagemagick
          ffmpeg-full
          # file/archive management
          ouch
          p7zip
          rclone
          trash-cli
          libarchive
          unzip
          unrar
          zip
          # misc
          age
          calc
          git
          wget
          nurl
        ]
        ++ (extlib.armOrNot [
          rar
        ] []);
    })
  ];
  home = [
    {
      programs = {
        btop.enable = true;
        direnv.enable = true;
        fzf.enable = true;
        zathura.enable = true;
      };
      xdg.mimeApps.defaultApplications."application/pdf" = "org.pwmt.zathura.desktop";
    }
  ];
}
