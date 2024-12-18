HOSTNAME="${1}"
sudo nixos-generate-config --show-hardware-config > $HOME/nixos-dotfiles/modules/hosts/${HOSTNAME}/hardware.nix