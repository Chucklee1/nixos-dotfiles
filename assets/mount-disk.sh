msg-sleep "mounting other disk"

OTHER_DISK=("BLUE_SATA" "AMONG_US")
for DISK in "${OTHER_DISK[@]}"; do
  lsblk -f | grep "$DISK" >/dev/null
  if [ $? -eq 0 ]; then
    msg-sleep "disk $DISK found"
    sudo mkdir -p /mnt/media/${USER}/${DISK}
    msg-sleep "created directory for $DISK"
    sudo mount -L $DISK /media/${USER}/${DISK}
  else
    echo "disk $DISK not found, skipping step"
  fi
done