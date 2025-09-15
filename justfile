nix_cmd := "nix --extra-experimental-features 'nix-command flakes'"
build_flags := "--impure --show-trace"
pwd := env("PWD")
mdir := "/mnt"

# installation

format layout device:
    sudo {{nix_cmd}} run github:nix-community/disko -- \
        --mode disko {{pwd}}/assets/disko/{{layout}}.nix \
        --arg device '"{{device}}"'

show-hardware:
    sudo nixos-generate-config \
        --show-hardware-config \
        --root {{mdir}}

install profile:
    sudo nixos-install \
        --root {{mdir}} \
        --flake {{pwd}}#{{profile}} \
        {{build_flags}}

# General

update:
    nix-shell -p gh --run \
        nix flake update \
        --option access-tokens "github.com=$(gh auth token)"

rebuild profile:
    sudo nixos-rebuild switch \
        --flake {{pwd}}#{{profile}} \
        {{build_flags}}; \

# btrfs
snapshot:
    sudo {{pwd}}/assets/scripts/mkSnapshot
