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

- overhaul module system split into the following:
  config (module definitions)
  profiles (machine declaration of configs) - only moving things around, recursive import
  and profiles will stay the same
- re-add api support back to navidrome service
- get sops to work with desktop and laptop profiles
- find a way to sync ungoogled chromium profiles

# **CMD cheatsheet**
- sudo nix --experimental-features "nix-command flakes" run github:nix-community/disko -- --mode disko /tmp/disko.nix --arg device '"/dev/vda"'
