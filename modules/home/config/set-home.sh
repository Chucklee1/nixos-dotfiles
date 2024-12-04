#!/bin/bash

CONFIG_DIR="~/.config"
MODULE_DIR="~/nixos-dotfiles/modules/home/config"
IMAGE="~/nixos-dotfiles/pictures/mono-forest.PNG"

TASK_ARR=("dunst" "nm-applet" "wlsunset" "xwayland-satellite" "waybar" "swww")
# kill task

for TASK in "${TASK_ARR[@]}"; do
    pkill "$TASK"
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
if [ -d "$CONFIG_DIR/niri" ]; then
    rm -rf $CONFIG_DIR/niri/*
else
    mkdir $CONFIG_DIR/niri
fi

ln -s $MODULE_DIR/niri/config.kdl $CONFIG_DIR/niri/



# start task
dunst
nm-applet
wlsunset -t 5000 -T 6500
xwayland-satellite
waybar &
swww-daemon
swww img $IMAGE
