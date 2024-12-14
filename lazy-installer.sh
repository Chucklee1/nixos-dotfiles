DISK="/dev/${1}"
PART="sudo parted ${DISK} --"
msg-sleep() {
    local msg="$1"
    echo "${msg}" 
    sleep 2
}

msg-sleep "partitioning ${DISK}..."

$PART mklabel gpt
msg-sleep "created gpt label"

$PART mkpart ESP fat32 1MB 1G
$PART set 1 esp on
msg-sleep "created boot partition"

$PART mkpart root 1G 100%
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
sudo nixos-generate-config --show-hardware-config --root /mnt > /home/nixos/nixos-dotfiles/modules/machines/desktop.nix
msg-sleep "installing nixos"
sudo nixos-install --flake /home/nixos/nixos-dotfiles/#desktop

