#!/bin/bash

CONFIG_DIR="~/.config"
MODULE_DIR="~/nixos-dotfiles/modules"
IMAGE="~/nixos-dotfiles/pictures/mono-forest.PNG"

TASK_ARR=("dunst" "nm-applet" "wlsunset" "xwayland-satellite" "waybar" "swww")
# kill task

for TASK in "${TASK_ARR[@]}"; do
    pkill "$TASK"
done

# symlink
if [ -d "$CONFIG_DIR/waybar" ]; then
    rm -rf $CONFIG_DIR/waybar/*
else
    mkdir $CONFIG_DIR/waybar
fi

ln -s $MODULE_DIR/niri/waybar/config.jsonc $CONFIG_DIR/waybar/
ln -s $MODULE_DIR/niri/waybar/style.css $CONFIG_DIR/waybar/

# start task
dunst
nm-applet
wlsunset -t 5000 -T 6500
xwayland-satellite
waybar &
swww-daemon
swww img $IMAGE
