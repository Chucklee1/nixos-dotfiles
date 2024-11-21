#!/bin/bash

rm -rf $HOME/.config/waybar/*
ln -s $HOME/nixos-dotfiles/modules/wayland/waybar/config.jsonc $HOME/.config/waybar/
ln -s $HOME/nixos-dotfiles/modules/wayland/waybar/style.css $HOME/.config/waybar/

rm -rf $HOME/.config/hypr/*
ln -s $HOME/nixos-dotfiles/modules/wayland/hyprland/hyprland.conf $HOME/.config/hypr/