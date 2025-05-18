# ---- scripting ----

- for loop `for i in $cond; do $function; done`
- archive `tar czvf [name].tar.gz [folder]/\*`

# ---- ffmpeg ----

- general recipe `ffmpeg -i name.wav name.m4a`
- ffmpeg -i (input flag) <media-file> <output-media-file>

## common options

- `-c:<stream-type> <codec>`
- stream-types:
  - v -> (v)ideo
  - a -> (a)udio
- codec:
  - codec name, duh
  - common codecs:
    - aac for media like .m4a

## misc:

-vn -> remove video

/_margin formatting_/
#item {
margin: [TOP]px [RIGHT]px [BOTTOM]px [LEFT]px;
margin: [TOP]px [LEFT & RIGHT]px [BOTTOM]px;
}
