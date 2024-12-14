DISK = "/dev/${1}"

echo "the disk is
#parted $DISK -- mklabel gpt
#parted $DISK -- mkpart ESP fat32 1MB 1G
#parted $DISK -- set 1 esp on
#parted $DISK -- mkpart root ext4 1G 