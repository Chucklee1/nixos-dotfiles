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
- Umbra profile is down as of now, reworking for other uses since
  the NixOS live-installer solved most of the things I wanted
- Otherwise, you can just clone this repo and try to work things
  out via my justfile. Almost everything you would need to install
  your system is in there, the only thing you would need to do is add
  a configuration file for your system (under hosts/).

## TODO:
- [x] Continue to be as indecisive as possible
- [x] Find replacement for horrid recursiveMerge in libs.nix
- [ ] Make neovim not look like poop
- [ ] ~~Nix Flake Parts?~~ no
- [ ]
### Lower Priority:
- Umbra features:
  - [ ] aarch64 support
