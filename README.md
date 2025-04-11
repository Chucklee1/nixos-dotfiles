|personal dotfiles for my machines|

# **notes**

- these notes are mostly here for my own sake
- default order parameters -> { lib, config, pkgs, inputs, specialArgs, ... }
- **general module layout:**
  - everything under self/modules is recursivly imported from  
    if-statement hell function in flake
  - module system:
    - module-name then module-type (nix or home)
    - current module names: global, laptop, desktop
    - global is merged with laptop & desktop

# **TODO**

- remove profiles
- keep nix/home seperations

# **host**

- desktop -> yggdrasil
- laptop -> nimbus
- MacBook Pro -> darwin

# **CMD cheatsheet**

- sudo nix --experimental-features "nix-command flakes" run github:nix-community/disko -- --mode disko /tmp/disko.nix --arg device '"/dev/vda"'
