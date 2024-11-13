{
  pkgs,
  inputs,
  niri,
}: {nixpkgs.overlays = [niri.overlays.niri];}
