#!/bin/bash

CONFIG_DIR="~/.config"
MODULE_DIR="~/nixos-dotfiles/modules"
IMAGE="~/nixos-dotfiles/pictures/mono-forest.PNG"

# kill task
pkill waybar

# symlink
if [ -d "$CONFIG_DIR/waybar" ]; then
    rm -rf $CONFIG_DIR/waybar/*
else
    mkdir $CONFIG_DIR/waybar
fi

ln -s $MODULE_DIR/niri/waybar/config.jsonc $CONFIG_DIR/waybar/
ln -s $MODULE_DIR/niri/waybar/style.css $CONFIG_DIR/waybar/

# start task
waybar &
swww img $IMAGE
