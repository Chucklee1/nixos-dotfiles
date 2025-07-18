PaperWM = hs.loadSpoon("PaperWM")

PaperWM.window_gap = 1
PaperWM.swipe_fingers = 3

PaperWM:bindHotkeys({
  focus_left           = { { "alt" }, "left" },
  focus_right          = { { "alt" }, "right" },
  focus_up             = { { "alt" }, "up" },
  focus_down           = { { "alt" }, "down" },

  swap_left            = { { "alt", "shift" }, "left" },
  swap_right           = { { "alt", "shift" }, "right" },
  swap_up              = { { "alt", "shift" }, "up" },
  swap_down            = { { "alt", "shift" }, "down" },

  full_width           = { { "alt" }, "f" },
  cycle_width          = { { "alt" }, "r" },
  reverse_cycle_width  = { { "alt", "ctrl" }, "r" },
  cycle_height         = { { "alt", "shift" }, "r" },
  reverse_cycle_height = { { "alt", "ctrl", "shift" }, "r" },

  increase_width       = { { "alt" }, "l" },
  decrease_width       = { { "alt" }, "h" },

  slurp_in             = { { "alt" }, "i" },
  barf_out             = { { "alt" }, "o" },

  toggle_floating      = { { "alt", "shift" }, "escape" },

  focus_window_1       = { { "alt", "shift" }, "1" },
  focus_window_2       = { { "alt", "shift" }, "2" },
  focus_window_3       = { { "alt", "shift" }, "3" },
  focus_window_4       = { { "alt", "shift" }, "4" },
  focus_window_5       = { { "alt", "shift" }, "5" },
  focus_window_6       = { { "alt", "shift" }, "6" },
  focus_window_7       = { { "alt", "shift" }, "7" },
  focus_window_8       = { { "alt", "shift" }, "8" },
  focus_window_9       = { { "alt", "shift" }, "9" },

  switch_space_l       = { { "alt" }, "," },
  switch_space_r       = { { "alt" }, "." },
  switch_space_1       = { { "alt" }, "1" },
  switch_space_2       = { { "alt" }, "2" },
  switch_space_3       = { { "alt" }, "3" },
  switch_space_4       = { { "alt" }, "4" },
  switch_space_5       = { { "alt" }, "5" },
  switch_space_6       = { { "alt" }, "6" },
  switch_space_7       = { { "alt" }, "7" },
  switch_space_8       = { { "alt" }, "8" },
  switch_space_9       = { { "alt" }, "9" },

  move_window_1        = { { "alt", "shift" }, "1" },
  move_window_2        = { { "alt", "shift" }, "2" },
  move_window_3        = { { "alt", "shift" }, "3" },
  move_window_4        = { { "alt", "shift" }, "4" },
  move_window_5        = { { "alt", "shift" }, "5" },
  move_window_6        = { { "alt", "shift" }, "6" },
  move_window_7        = { { "alt", "shift" }, "7" },
  move_window_8        = { { "alt", "shift" }, "8" },
  move_window_9        = { { "alt", "shift" }, "9" }
})
PaperWM:start()
