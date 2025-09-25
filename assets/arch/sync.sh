#!/usr/bin sh

ASSET_DIR=$HOME/nixos-dotfiles/assets/arch
# in case xdg isn't configured yet
CFG_DIR=$HOME/.config

for i in "$ASSET_DIR"; do
	ln -s "$ASSET_DIR/${i##*/}" "$CFG_DIR/${i##*/}"
