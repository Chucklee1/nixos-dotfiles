# personal dotfiles for my machines

## machines:

- desktop: desktop
- macbook: macbook
- umbra: vm testing

## TODO:

- [x] continue to be as indecisive as possible
- [ ] find replacement for horrid recursiveMerge in libs.nix
- [ ] Work on iso image profile for easy booting onto any computer (still thinking of good name)
- [ ] work on expanding umbra functionality

## Want to use my setup?

- on metal:

    - clone this repo with `git clone https://github.com/Chucklee1/nixos-dotfiles`
    - make sure you are in the repository directory so you can use just
    - format disk:
        - `just format <layout> <device>`
        - layout expands to `pwd/assets/disko/<layout>.nix` (currently ext4 & desktop)
        - device can be any format such as `/dev/sdx` or `/dev/disk/by-uuid/sdx`
    - get hardware info:
        - `just show-hardware`
        - from there you can add that info to hardware.nix or use a preset
    - install system: `just install <profile>`

- virtual machine:

    - run `nix run github:Chucklee1/nixos-dotfiles#<profile>`
    - add `--extra-experimental-features 'nix-command flakes'` after nix if the command did not work
    - umbra is currently the only vm profile

- If 1st time installing, download the repo and link the hardware config properly
- If you using the installer on a USB stick smaller than ~5-10 GB, make sure to disable niri, then build again with niri enabled or the installer will freeze

Credits to _~stolen~_ taken functions from sodiboos config at github.com:sodiboo/system
