# NOTE: this is just a lazy instruction script for
# my desktop, not for normal usage, just ignore this

USERNAME="goat"
HOSTNAME="$USERNAME-archtop"

# in liveboot
pacman -S \
    base base-devel \
    linux linux-firmware \
    efibootmgr grub \
    btrfs-progs grub-btrfs inotify-tools \
    networkmanager \
    sudo man openssh git \
    pacman-contrib reflector \
    amd-ucode nvidia \
    pipewire pipewire-pulse pipewire-alsa pipewire-jack \
    neovim zsh

genfstab -U /mnt >>/mnt/etc/fstab

# chroot
arch-chroot /mnt

ln -sf /usr/share/ZONE/CITY /etc/localtime
hwclock --systohc

echo "en_US.UTF-8 UTF-8" >>/etc/locale.gen
local-gen
echo "en_US.UTF-8" >>/etc/locale.conf

echo "$HOSTNAME" >>/etc/hostname
printf "%s\n%s\n%s" "127.0.0.1 localhost" "::1 localhost" "127.0.1.1 $HOSTNAME" >>/etc/hosts

useradd -mG wheel "$USERNAME"
# remember: run passwd and passwd user after
# also comment out %wheel ALL=(ALL:ALL) ALL

grub-install --target=x86_64-efi --efi-directory=/boot/efi --bootloader-id=ARCH
grub-mkconfig -o /boot/grub/grub.cfg

systemctl enable NetworkManager 
exit
