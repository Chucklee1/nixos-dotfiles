#!/bin/bash

CONFIG_DIR="$HOME/.config"
MODULE_DIR="$HOME/nixos-dotfiles/modules/home/config"
IMAGE="$HOME/nixos-dotfiles/pictures/mono-forest.PNG"

TASK_ARR=("waybar" "swww")
# kill task

for TASK in "${TASK_ARR[@]}"; do
    killall "$TASK"
done

# symlinking

# waybar
if [ -d "$CONFIG_DIR/waybar" ]; then
    rm -rf $CONFIG_DIR/waybar/*
else
    mkdir $CONFIG_DIR/waybar
fi

ln -s $MODULE_DIR/waybar/config.jsonc $CONFIG_DIR/waybar/
ln -s $MODULE_DIR/waybar/style.css $CONFIG_DIR/waybar/

# niri
#if [ -d "$CONFIG_DIR/niri" ]; then
#    rm -rf $CONFIG_DIR/niri/*
#else
#    mkdir $CONFIG_DIR/niri
#fi

#ln -s $MODULE_DIR/niri/config.kdl $CONFIG_DIR/niri/


waybar &
swww-daemon
swww img $IMAGE
