DISK="/dev/${1}"

echo "partitioning disk ${DISK}"
parted $DISK -- mklabel gpt
parted $DISK -- mkpart ESP fat32 1MB 1G
parted $DISK -- set 1 esp on
parted $DISK -- mkpart root ext4 1G 
sleep 2

echo "formatting disk ${DISK}"
mkfs.fat -F32 -n BOOT ${DISK}p1
mkfs.ext4 -L NIXOS-ROOT ${DISK}p2
sleep 2

echo "mounting root partition to /mnt"
mount ${DISK}p2 /mnt
echo "creating boot directory"
mkdir /mnt/boot
echo "mounting boot partition to /mnt/boot"
mount ${DISK}p1 /mnt/boot
sleep 2

echo "generating hardware config"
sudo nixos-generate-config --only-hardware > ./modules/machines/desktop.nix
echo "installing nixos"
nixos-install --flake ./#desktop

