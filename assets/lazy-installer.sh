DISK="/dev/${1}"
PROFILE="${2}"
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

msg-sleep "mounting partitions..."
sudo mount ${DISK}p2 /mnt
msg-sleep "mounted root to /mnt"
sudo mkdir /mnt/boot
msg-sleep "created directory /mnt/boot"
sudo mount ${DISK}p1 /mnt/boot
msg-sleep "mounted boot to /mnt/boot"

msg-sleep "generating hardware config"
sudo nixos-generate-config --show-hardware-config --root /mnt > $HOME/nixos-dotfiles/modules/hosts/${PROFILE}/hardware.nix
msg-sleep "generated to desktop module"
msg-sleep "installing nixos"
sudo nixos-install --flake $HOME/nixos-dotfiles/#${PROFILE}
msg-sleep "completed!"
