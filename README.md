personal dotfiles for my machines

notes:
- default order parameters -> { lib, config, pkgs, inputs, specialArgs, ... }
- hastag next to imports path = toggle module
- using lazy-installer:
    - command ->
      nix run --extra-experimental-features 'nix-command flakes' //
      github:Chucklee1/nixos-dotfiles#lazy-installer //
      -- "<disk>" "<profile>"
    - <disk-name> -> eg: sdaX, nvmeXnY, vdaX
    - <profile-name> -> profile under flake.nix, mine are laptop and desktop. you could probably add your own but I only know this works for my machines
