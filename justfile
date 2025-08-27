nix_cmd := "nix --extra-experimental-features 'nix-command flakes'"
pwd := env("PWD")
live_root := "/mnt"
build_flags := "--impure --show-trace"

format layout device:
    sudo {{nix_cmd}} run github:nix-community/disko -- \
        --mode disko {{pwd}}/assets/disko/{{layout}}.nix \
        --arg device '"{{device}}"'

show-hardware:
    sudo nixos-generate-config \
        --no-filesystems --show-hardware-config \
        --root {{live_root}}

install profile:
    sudo nixos-install \
        --root {{live_root}} \
        --flake {{pwd}}#{{profile}} \
        {{build_flags}}

update:
    nix flake update

rebuild profile:
    if [ "{{profile}}" = "macbook" ]; then \
        sudo darwin-rebuild switch \
            --flake {{pwd}}#{{profile}} \
            {{build_flags}}; \
    else \
        sudo nixos-rebuild switch \
            --flake {{pwd}}#{{profile}} \
            {{build_flags}}; \
    fi