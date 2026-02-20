{extlib, ...}: {
  nix = [
    ({pkgs, ...}: {
      environment.systemPackages = with pkgs;
        [
          # build essentials
          gcc
          gnumake
          cmake
          ninja
          meson
          autoconf
          pkg-config
          # misc
          just
          lua
          python3
          # lsps
          asm-lsp # GAS/GO assembly
          clang-tools # c/c++
          cmake-language-server
          bash-language-server
          lemminx # xml
          lua-language-server
          marksman
          nixd
          typescript-language-server
          vscode-langservers-extracted
          # diagnostics
          statix
          # formatters
          alejandra
          html-tidy
          nodePackages.prettier
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
          wget
          nurl
        ]
        ++ (extlib.armOrNot [
          rar
          # broken on aarch64
          gdb
        ] []);
    })
    (extlib.darwinOrLinux {} {programs.nix-ld.enable = true;})
  ];
  home = [
    {
      programs = {
        btop.enable = true;
        direnv.enable = true;
        fzf.enable = true;
        zathura.enable = true;
      };
    }
  ];
}
