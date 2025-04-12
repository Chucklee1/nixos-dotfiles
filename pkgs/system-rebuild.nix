{ pkgs }:

pkgs.writeShellApplication {
  name = "system-rebuild";
  text = ''
    SYSTEM=$1
    if [SYSTEM != "darwin"]; then
      sudo nixos-rebuild switch --impure --show-trace --flake "$HOME/nixos-dotfiles#$SYSTEM"
    else then
      sudo nixos-darwin switch --impure --show-trace --flake $HOME/nixos-dotfiles#darwin
  '';
}