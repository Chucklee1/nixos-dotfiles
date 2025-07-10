# personal dotfiles for my machines

## machines:

- desktop: desktop
- macbook: macbook
- umbra: usb profile _(see todo)_

## TODO:

- [x] continue to be as indecisive as possible
- [ ] find replacement for horrid recursiveMerge in libs.nix
- [x] move weird external args to specialArgs
- [x] Changes to waybar, spacing in windows, and corner rounding niri
- [ ] work on iso/vm profile umbra with script for vm and config for testing

## Want to use my setup?

1: Format Disko

- /path/to/disko.nix: Find disko templates in my assets/disko folder or on their repo page (github.com/nix-community/disko)
- /dev/device: Replace w/ disk you want to format, I have not tried anything like /dev/disk/by-uuid so pls let me know how that goes
- Otherwise just run the command below and it works

```
  sudo nix --experimental-features "nix-command flakes" run \
   github:nix-community/disko -- \
   --mode disko /path/to/disko.nix \
   --arg device '"/dev/device"'
```

2: Generate Hardware config (TODO make vm profile so you can skip this)
`sudo nixos-generate-config --no-filesystems --show-hardware-config --root /mnt`

- You can then either put it in as an import or add it into my config
- install to /mnt

3: Install System
`"sudo nixos---flake install --root /mnt --flake github:Chucklee1/nixos-dotfiles"`

- If 1st time installing, download the repo and link the hardware config properly
- If you using the installer on a USB stick smaller than ~5-10 GB, make sure to disable niri, then build again with niri enabled or the installer will freeze

Credits to _~stolen~_ taken functions from sodiboos config at github.com:sodiboo/system
