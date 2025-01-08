|personal dotfiles for my machines|

**notes**
- these notes are mostly here for my own sake
- default order parameters -> { lib, config, pkgs, inputs, specialArgs, ... }
- hastag next to imports path -> toggle module
- **general module layout (for now):**
    - system module stores general attribute declarations and options
    - configs folder modules store most attribute delacations with the 
      exception of simple system configs that dont make much sense to
      have in their own module; at least to me that is...
    - the *host/<hostname>* modules host stores the auto generated hardware
      modules, the config module sets the overrides for those hardware 
      module the host config module 
- **using lazy-installer:**
    - command ->
      *$ nix run --extra-experimental-features 'nix-command flakes' //
      github:Chucklee1/nixos-dotfiles#lazy-installer //
      -- "<disk>" "<profile>"*
    - <disk-name> -> eg: sdaX, nvmeXnY, vdaX
    - <profile-name> -> profile under flake.nix, mine are laptop and desktop. 
      you could probably add your own but I only know this works for my machines
- **ssh notes:**
    - generate key -> *$ ssh-keygen*
    - pair key to remote -> *ssh-copy-id -i ./.ssh/<name-of-ssh-keygen-file>.pub //
      <remotename>@<remoteip>*
- **patching commands:**
    - git format-patch --stdout HEAD~2..HEAD > $HOME/nixos-dotfiles/assets/dwm-override.patch 
