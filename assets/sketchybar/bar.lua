local colors = require("colors")

-- Equivalent to the --bar domain
sbar.bar({
  height = 32,
  color = colors.bar.bg,
  border_color = colors.bar.border,
  shadow = false,
  sticky = false,
  padding_right = 10,
  padding_left = 10,
  blur_radius = 20,
  topmost = "window",
})
