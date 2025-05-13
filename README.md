|personal dotfiles for my machines|

# notes

- these notes are mostly here for my own sake
- default order parameters -> { lib, config, pkgs, inputs, specialArgs, ... }
- **general module layout:**
  - everything under self/modules is recursivly imported from  
    if-statement hell function in flake
  - module system:
    - module-name then module-type (nix or home)
    - current module names: global, desktop, and macbook
    - global is merged with macbook & desktop

# cheatsheet

- tar czvf [name].tar.gz [folder]/\*
- ## CSS
  - margin: [TOP]px [RIGHT]px [BOTTOM]px [LEFT]px;
  - margin: [TOP]px [LEFT & RIGHT]px [BOTTOM]px;
