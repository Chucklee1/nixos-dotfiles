{
  home.global = [
    ({pkgs, ...}: {
      home.packages = with pkgs; [
        # dev tools
        openai
        rclone
        python3
        gnumake
        gcc
        gdb # GNU Project debugger
      ];
      programs.direnv = {
        enable = true;
        enableBashIntegration = true; # see note on other shells below
      };
    })
  ];
  nix.desktop = [
    ({pkgs, ...}: {
      programs.nix-ld = {
        enable = true;
        libraries = with pkgs; [
          jq
          unzip
          python313
          python313Packages.pip
        ];
      };
    })
  ];
}
