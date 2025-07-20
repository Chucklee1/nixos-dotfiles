REPO="$(pwd)/nixos-dotfiles"

install() {
    sudo nixos-install --root /mnt --flake "$REPO"
    read -pr "Before entering chroot to set user password, input username: " username
    sudo nixos-enter --root /mnt -c "passwd $username"
}

format() {
    sudo nix --experimental-features "nix-command flakes" run \
        github:nix-community/disko -- \
        --mode disko "$REPO/assets/disko/$1.nix" \
        --arg device $2

    sudo nixos-generate-config --no-filesystems --show-hardware-config --root /mnt
}

if declare -f "$1" >/dev/null; then
    "$@"
else
    echo "Function '$1' not found."
    exit 1
fi
