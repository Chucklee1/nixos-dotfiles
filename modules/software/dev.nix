{extlib, ...}: {
  nix = [
    ({pkgs, ...}: {
      environment.systemPackages = with pkgs; [
        # haker
        just
        gcc gdb
        lua python3
        # lsps
        bash-language-server
        lua-language-server
        marksman
        nixd
        # info helpers
        ripgrep fzf fd pciutils zoxide
        # shells
        bash nushell
        # media
        exiftool mediainfo poppler ueberzugpp
        imagemagick ffmpeg-full
        # file/archive management
        ouch p7zip rclone trash-cli
        unzip unrar zip
        # misc
        age calc curl
        # nix helpers
        nurl
      ] ++
      (extlib.darwinOrLinux [] [rar]);
    })
    ({machine, pkgs, ...}:
      if machine == "umbra" then {}
      else {
        environment.systemPackages = with pkgs; [
          # lsps
          asm-lsp # GAS/GO assembly
          clang-tools
          jdt-language-server # java
          lemminx # xml
          kdePackages.qtdeclarative
          typescript-language-server
          vscode-langservers-extracted
          # diagnostics
          statix
          # formatters
          alejandra
          html-tidy
          nodePackages.prettier
          shfmt
        ];
      })
    (extlib.darwinOrLinux {} {programs.nix-ld.enable = true;}
    )
  ];
  home = [{
    programs = {
      btop.enable = true;
      direnv.enable = true;
      fzf.enable = true;
      zathura.enable = true;
    };
  }];
}
