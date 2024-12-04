#!/bin/bash

CONFIG_DIR="$HOME/.config"
FLAKE_DIR="$HOME/nixos-dotfiles"

TASK_ARR=("waybar" "swww")
# kill task

for TASK in "${TASK_ARR[@]}"; do
    killall "$TASK"
done

# symlinking

# waybar
mkdir $CONFIG_DIR/waybar
rm -rf $CONFIG_DIR/waybar/*
ln -s $FLAKE_DIR/dotconfig/waybar/* $CONFIG_DIR/waybar/

# pictures 
rm -rf $HOME/Pictures/*
ln -s $FLAKE_DIR/Pictures/* $HOME/Pictures/

waybar &
swww-daemon
swww img $HOME/Pictures/mono-forest.PNG
