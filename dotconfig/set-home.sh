#!/bin/bash

CONFIG_DIR="$HOME/.config"
MODULE_DIR="$HOME/nixos-dotfiles/dotconfig"
IMAGE="$HOME/Pictures/mono-forest.PNG"

TASK_ARR=("waybar" "swww")
# kill task

for TASK in "${TASK_ARR[@]}"; do
    killall "$TASK"
done

# symlinking

# waybar
mkdir $CONFIG_DIR/waybar
rm -rf $CONFIG_DIR/waybar/*

ln -s $MODULE_DIR/waybar/config.jsonc $CONFIG_DIR/waybar/
ln -s $MODULE_DIR/waybar/style.css $CONFIG_DIR/waybar/

# pictures directory
rm -rf $HOME/pictures 
ln -s $HOME/nixos-dotfiles/Pictures $HOME/

waybar &
swww-daemon
swww img $IMAGE
