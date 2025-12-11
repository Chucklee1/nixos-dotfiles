# nixos-dotfiles

- Personal dotfiles for my machines
- Credits to ~~stolen~~ taken functions from sodiboos config at <github.com:sodiboo/system>

## machines:
| name     | system/platform            | desc                  |
|----------|----------------------------|-----------------------|
| desktop  | x86-64-linux               | personal              |
| macbook  | aarch64-darwin (m4 silcon) | work & school         |
| umbra    | x86-64-linux               | vm & custom installer |
| inspiron | x86-64-linux               | server                |
| laptop   | x86-64-linux               | malware testing       |

## Want to use my stuff?
- The umbra profile is meant to be used by anyone who is insterested in my setup, or wants a nice minimal but easy-to-use installer
- **How to use the vm profile?**
  - Run the following nix command
      `$ nix --extra-experimental-features 'nix-command flakes' run github:Chucklee1/nixos-dotfiles#umbra`
  - Let nix build and set everything up, and it will startup an instance of qemu
  - That's it! Pretty Simple
+ **How to use the installer?**
  - Run a simillar command to the vm profile one, but with #installer instead of #umbra
       `$ nix --extra-experimental-features 'nix-command flakes' run github:Chucklee1/nixos-dotfiles#installer`
  - Build time will vary depending on hardware, though it should not take too long
  - Once done, the iso image will be in ./result/iso, grab that .iso file, and load it like you usually would when
    installing another OS (check nixos manual for details on burning an iso + loading the iso onto your computer if you do not know how to)

## TODO:
- [x] Continue to be as indecisive as possible
- [ ] Find replacement for horrid recursiveMerge in libs.nix
- [ ] Make neovim not look like poop
- [ ] Nix Flake Parts?
### Lower Priority:
  - Probably will need to write a custom c function :(
- [ ] Figure out how to write c functions for nix
- Umbra features:
  - [ ] aarch64 support
  - Add profile options for the following:
    - [ ] Use bash or sh instead of zsh
    - [ ] Disable stylix theming
    - [ ] Disable custom zsh prompt
    - [ ] Choose between neovim & emacs for default editor
  - [ ] Custom installer prompt at start (kind of like how arch does it)
