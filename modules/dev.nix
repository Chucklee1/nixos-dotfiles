{
  home.global = [
    {
      direnv = {
        enable = true;
        enableBashIntegration = true; # see note on other shells below
        #nix-direnv.enable = true;
      };
    }
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
