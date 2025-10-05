{
  nix = [
    ({pkgs, ...}: {
      environment.systemPackages = with pkgs; [
        # programs
        age
        calc
        curl
        ffmpeg-full
        gcc
        gdb # GNU debugger
        imagemagick
        just
        lua
        python3
        # lsps
        bash-language-server
        lua-language-server
        marksman
        nixd
      ];
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
        programs.nix-ld.enable = true;
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
    }
  ];
}
