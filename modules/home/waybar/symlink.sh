#!/bin/bash

rm -rf $HOME/.config/waybar/*
ln -s $HOME/nixos-dotfiles/modules/home/waybar/config.jsonc $HOME/.config/waybar/
ln -s $HOME/nixos-dotfiles/modules/home/waybar/style.css $HOME/.config/waybar/