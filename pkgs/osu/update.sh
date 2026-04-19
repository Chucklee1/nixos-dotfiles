#!/usr/bin/env bash
set -euo pipefail

new_tag="$(curl -s https://api.github.com/repos/ppy/osu/releases/latest | jq -r '.name')"
new_version="${new_tag%-lazer}"

jq -n --arg v "$new_version" '{version:$v}' > tmp.json

for pair in \
  "aarch64-darwin osu.app.Apple.Silicon.zip" \
  "x86_64-darwin osu.app.Intel.zip" \
  "x86_64-linux osu.AppImage"
do
  set -- $pair

  out=$(nix store prefetch-file \
    "https://github.com/ppy/osu/releases/download/$new_tag/$2" \
    --json)

  hash=$(jq -r '.hash // empty' <<<"$out")

  jq --arg sys "$1" --arg h "$hash" \
    '.[$sys]=$h' tmp.json > tmp2.json && mv tmp2.json tmp.json
done

mv tmp.json lock.json
