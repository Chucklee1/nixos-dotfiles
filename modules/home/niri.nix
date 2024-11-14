{pkgs, ...}: {
  #nixpkgs.overlays = [inputs.overlays.niri];
  programs.niri = {
    enable = true;
  };
}
