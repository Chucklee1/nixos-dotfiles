DISK="/dev/${1}"

msg-sleep() {
    local msg="$1"
    sleep 4
    printf "--------------------\n-- %s --\n--------------------\n" "$msg"
}

msg-sleep "partitioning disk ${DISK}"

parted $DISK -- mklabel gpt
msg-sleep "created gpt label"

parted $DISK -- mkpart ESP fat32 1MB 1G
parted $DISK -- set 1 esp on
parted "created boot partition"

parted $DISK -- mkpart root ext4 1G 100%
msg-sleep "created root partition"


msg-sleep "formatting disk ${DISK}"

mkfs.fat -F32 -n BOOT ${DISK}p1
msg-sleep "formatted BOOT"

mkfs.ext4 -L NIXOS-ROOT ${DISK}p2
msg-sleep "formatted NIXOS-ROOT"


msg-sleep "mounting root partition to /mnt"
mount ${DISK}p2 /mnt
msg-sleep "mounted"

msg-sleep "creating boot directory"
mkdir /mnt/boot
msg-sleep "mounted"

msg-sleep "mounting boot partition to /mnt/boot"
mount ${DISK}p1 /mnt/boot
msg-sleep "mounted"

msg-sleep "generating hardware config"
sudo nixos-generate-config --only-hardware > ./nixos-dotfiles/modules/machines/desktop.nix
msg-sleep "installing nixos"
nixos-install --flake ./nixos-dotfiles/#desktop

