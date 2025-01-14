|personal dotfiles for my machines|

# ** notes **
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

# **function/command names I can't remember**

## custom flake defined
  - lazy-installer:
    ```
    $ nix run --extra-experimental-features 'nix-command flakes' //
    github:Chucklee1/nixos-dotfiles#lazy-installer //
    -- "<disk>" "<profile>" 
    ```
    - <disk-name> -> eg: sdaX, nvmeXnY, vdaX
    - <profile-name> -> profile under flake.nix, mine are laptop and desktop. 

## ssh notes
  - generate key: 
    ```     
    $ ssh-keygen* 
    ```
  - pair key to remote:
    ```
    *ssh-copy-id -i ./.ssh/<name-of-ssh-keygen-file>.pub //
    <remotename>@<remoteip>*
    ```

## patching commands
  - git patching: 
    git git format-patch --stdout HEAD~2..HEAD > // 
     /path/to/patch.patch
  

## nix functions
- lib.concatList
    - merges multiple list concatenating,
      or linking them together
    - eg usage:
      ``` 
      let
        lists =[ [a b c] [ d e f ] ];
      in
        lib.concatLists lists
       ```    
      output: [ a b c d e f ]

- lib.foldl'
    - Iteratively applies function to reduce list into single value
    - takes 3 values in order:
      >*func, initVal, list*
    - foldl' follows the following recursive sequence, where 
      append is a made-up function initalValue = 1:
      ```
      lib.foldl(a: b: append a to b) 1 [2, 3, 4, 5]
      ```
    - heres the same function in c-ish syntax: 
      
