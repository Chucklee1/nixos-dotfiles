DISK="/dev/${1}"

msg-sleep() {
    local msg="$1"
    echo "${msg}" 
    sleep 2
}

msg-sleep "partitioning ${DISK}..."

sudo parted $DISK -- mklabel gpt
msg-sleep "created gpt label"

sudo parted $DISK -- --script mkpart ESP fat32 1MB 1G
sudo parted $DISK -- --script set 1 esp on
sudo parted "created boot partition"

sudo parted $DISK -- --script mkpart root 1G 100%
msg-sleep "created root partition"


msg-sleep "formatting ${DISK}..."

sudo mkfs.fat -F32 -n BOOT ${DISK}p1
msg-sleep "formatted BOOT"

sudo mkfs.ext4 -L NIXOS-ROOT ${DISK}p2
msg-sleep "formatted NIXOS-ROOT"


msg-sleep "mounting root partition to /mnt"
sudo mount ${DISK}p2 /mnt
msg-sleep "success"

msg-sleep "creating boot directory"
sudo mkdir /mnt/boot
msg-sleep "success"

msg-sleep "mounting boot partition to /mnt/boot"
sudo mount ${DISK}p1 /mnt/boot
msg-sleep "success"

msg-sleep "generating hardware config"
sudo nixos-generate-config --only-hardware --root /mnt > /home/nixos/nixos-dotfiles/modules/machines/desktop.nix
msg-sleep "installing nixos"
sudo nixos-install --flake /home/nixos/nixos-dotfiles/#desktop

