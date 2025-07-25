# variables
REPO="$HOME/nixos-dotfiles"
if [ ! -d "$REPO" ]; then
    REPO="github:Chucklee1/nixos-dotfiles"
fi
NIX_CMD="nix --experimental-features 'nix-command flakes'"
NIX_RUN="$NIX_RUN run"

# print helpers
msg() {
    case "$1" in
    head) printf "\e[1;4m%s\e[0m\n" "$2" ;;
    opt) printf "  %s\n" "$2" ;;
    subopt) printf "    %s\n" "$2" ;;
    *) echo "$2" ;;
    esac
}
layouts() {
    for i in "$REPO"/assets/disko/*; do
        file=${i##*/}
        printf "%s " "${file%.*}"
    done
}

# logic functions - in system
rebuild() {
    local FLAGS="--impure --show-trace"
    sudo nixos-rebuild switch "$FLAGS" --flake "$REPO#$1"
}
update() {
    "$NIX_CMD" flake update "$REPO"
}

# logic function - liveboot
format() {
    local FLAGS="--mode disko $REPO/disko/$1.nix --arg device $DEVICE"
    sudo "$NIX_RUN" github:nix-community/disko -- "$FLAGS"
}
print-hardware() {
    local ROOT="/mnt"
    local FLAGS="--show-hardware --root $ROOT"
    sudo nixos-generate-config "$FLAGS"
}
install() {
    local ROOT="/mnt"
    local FLAGS="--root $ROOT --flake $REPO#$1 --impure --show-trace"
    sudo nixos-install "$FLAGS"
}

# ---- main script ----

case "$1" in
rebuild) rebuild "$@" ;;
update) update ;;
format) format "$@" ;;
install) install "$@" ;;
print-cfg) print-cfg "$@" ;;
-h | --help | *)
    msg head "Usage:"
    msg opt "$0 [CMD] [OPTIONS]"
    msg head "Options:"
    msg opt "-h, --help                 - Displays this message"
    msg head "Commands:"
    msg opt "rebuild <profile>          - Rebuild profile config"
    msg opt "update                     - Update flake inputs"
    msg opt "format <layout> <device>   - format disk, see below for more info..."
    msg opt "print-hardware             - outputs hardware configuration to the screen"
    msg opt "install <profile>          - installs given profile to system on /mnt"
    msg head "commands - format:"
    msg opt "<device>:                  - partition target in format '\"/dev/sdx\"'"
    msg opt "<layout>:                  - disk layout type, choose from: $(layouts)"
    msg opt "check '$REPO/assets/disko' for disk-layout definitions"
    exit 0
    ;;
esac
