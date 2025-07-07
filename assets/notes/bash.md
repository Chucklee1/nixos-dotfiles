# ---- general ----

- for loops:

```bash
    for item in $condition; do # example conditions include directories and lists of strings
        $function;
    done
```

- Output base-name of file without extension: `${name%*}`

# ---- ansi escape codes ----

- Format: `\e[#m]`
- Text
  | Style | # |
  | --------- | -- |
  | Reset | 0 |
  | Bold | 1 |
  | Italic | 3 |
  | Underline | 4 |
  | Red | 31 |
  | Green | 32 |
  | Yellow | 33 |
  | Blue | 34 |
  | Magenta | 35 |
  | Cyan | 36 |
  | White | 37 |

# ---- CLI ----

- Archive `tar czvf [name].tar.gz [folder]/\*`
- Convert image (imagemagick): `magick in.ext out.ext`
- Convert media (FFMPEG): `ffmpeg -i in.ext out.ext`

# --- FFMPEG Syntax ----

- codec:
  - `-c:a <codec>` or `-c:v <codec>`
  - `-c` refers to the codec-type-flag
  - part after the colon specifies which stream
  - a -> audio ; v -> video
  - `<codec>` where you would insert the codec
