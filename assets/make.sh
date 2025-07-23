# variables
REPO="$(pwd)/nixos-dotfiles"

NL="\e[0m\n" # reset + newline
ERROR="\e[1;31m" # bold red
WARN="\e[1;33m" # bold yellow
INFO="\e[3;33m" # italic yellow
SUCS="\e[3;32m" # bold green

layouts() {
    for i in "$REPO"/assets/disko/*; do
        file=${i##*/}
        echo "${file%.*}"
    done
}
pipe() {
    printf '%*s\n' 80 '' | tr ' ' '-'
}
# Function to display script usage
usage() {
    echo "Usage: $0 [OPTIONS]"
    pipe
    echo "Options:"
    echo " -h, --help         Display this help message"
    printf %s " -d, --device  partition target in format '\"/dev/sdx\"'"
    echo " -l, --layout       disk layout type (defaults to ext4)"
    pipe
    printf "%s\n%s\n%s\n" \
        "Current disk layouts:" \
        "$(layouts)" \
        "check repo-root/assets/disko for disk-layout definitions"
    pipe
}

# logic functions
install() {
    sudo nixos-install --root /mnt --flake "$REPO"
    read -pr "Do you wish to chroot to set a user password (Y/N)? " choice
    if $choice != 'Y' || 'y'; then
        printf "${SUCS}%s${NL}" "Setup complete! Exiting script..."
        exit 0
    else
        read -pr "Enter username: " username
        sudo nixos-enter --root /mnt -c "passwd $username"
    fi
}

format() {
    printf "${WARN}%s${NL}${INFO}%s${NL}${WARN}%s${NL}" \
        "WARNING: This will wipe your specified device" \
        "this means ALL DATA WILL BE LOST UPON FORMAT" \
        "do you wish to continue? (YES/abort)"

    if $choice != 'Y' || 'y'; then
        printf "${SUCS}%s${NL}" "Setup complete! Exiting script..."
        exit 0
    else
        read -pr "Enter username: " username
        sudo nixos-enter --root /mnt -c "passwd $username"
    fi
    sudo nix --experimental-features "nix-command flakes" run \
        github:nix-community/disko -- \
        --mode disko "$REPO/assets/disko/$1.nix" \
        --arg device $2

    sudo nixos-generate-config --no-filesystems --show-hardware-config --root /mnt
}
