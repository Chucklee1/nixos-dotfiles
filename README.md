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

- make server profile
- setup multi-user comp. for sops
- setup disko btrfs drive for backups
- rework current module system to be more flexible
- set up special arg settings hub in flake
