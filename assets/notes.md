# ---- scripting ----

- for loop `for i in $cond; do $function; done`
- archive `tar czvf [name].tar.gz [folder]/\*`

# ---- ffmpeg ----

- general recipe `ffmpeg -i name.wav name.m4a`
- ffmpeg -i (input flag) <media-file> <output-media-file>

## common options

- `-c:<stream-type> <codec>`
- stream-types:
  - v -> (v)ideo
  - a -> (a)udio
- codec:
  - codec name, duh
  - common codecs:
    - aac for media like .m4a
    - mpeg layers mp1 mp3 mp3 mp4
    - flac for optimized lossless
    - wav for pure uncompressed lossless

## misc:

-vn -> remove video

/_margin formatting_/
#item {
margin: [TOP]px [RIGHT]px [BOTTOM]px [LEFT]px;
margin: [TOP]px [LEFT & RIGHT]px [BOTTOM]px;
}

## ---- nix ----

- disko-generate
  ```
  sudo nix --experimental-features "nix-command flakes" run \
   github:nix-community/disko -- \
   --mode disko /path/to/disko.nix \
   --arg device '"dev/device"'
  ```
- show-hardware
  `sudo nixos-generate-config --no-filesystems --show-hardware-config --root /mnt`
- install to /mnt  
  `"sudo nixos---flake install --root /mnt --flake github:Chucklee1/nixos-dotfiles"`

- prefetcher with nurl: `nix run github:nix-community/nurl -- <link here>`

# ---- SOPS ----

- example .sops.yaml file

```yaml
keys:
  - &admin_alice 2504791468b153b8a3963cc97ba53d1919c5dfd4
  - &admin_bob age12zlz6lvcdk6eqaewfylg35w0syh58sm7gh53q5vvn7hd7c6nngyseftjxl
creation_rules:
  - path_regex: secrets/[^/]+\.(yaml|json|env|ini)$
    key_groups:
      - pgp:
          - *admin_alice
        age:
          - *admin_bob
```

- age keygen
  `age-keygen -o ~/.config/sops/age/keys.txt`
- add from ssh
  `$ nix-shell -p ssh-to-age --run 'ssh-keyscan example.com | ssh-to-age'`
  `$ nix-shell -p ssh-to-age --run 'cat /etc/ssh/ssh_host_ed25519_key.pub | ssh-to-age'`
- open secrets file
  `$ nix-shell -p sops --run "sops secrets/example.yaml"`
- add users to file
  `$ nix-shell -p sops --run "sops updatekeys secrets.yaml"`
