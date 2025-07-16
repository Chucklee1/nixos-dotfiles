build() {
  echo ""
}

install() {
  echo ""
}

update() {
  echo ""
}

if declare -f "$1" >/dev/null; then
  "$@"
else
  echo "Function '$1' not found."
  exit 1
fi
