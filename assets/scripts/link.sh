HOME_DIR="/media/goat/BLUE_SATA/home"
REPO_DIR="/media/goat/BLUE_SATA/repos"

for dir in "$HOME_DIR"/*; do
	ln -s "$dir" "$HOME"
done

ln -s "$REPO_DIR/personal" "$HOME"
ln -s "$REPO_DIR/nixos-dotfiles" "$HOME"
