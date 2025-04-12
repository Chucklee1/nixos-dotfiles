{ pkgs, ...}:
{
  system-rebuild = import ./system-rebuild.nix { inherit pkgs; };
}