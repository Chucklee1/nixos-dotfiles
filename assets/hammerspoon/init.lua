---- autoreload config ----
function reloadConfig(files)
    doReload = false
    for _,file in pairs(files) do
        if file:sub(-4) == ".lua" then
            doReload = true
        end
    end
    if doReload then
        hs.reload()
    end
end
myWatcher = hs.pathwatcher.new(os.getenv("HOME") .. "/.hammerspoon/", reloadConfig):start()
hs.alert.show("Config loaded")

---- load spoons ----
WarpMouse = hs.loadSpoon("WarpMouse")
PaperWM = hs.loadSpoon("PaperWM")

---- general spoon config ----
PaperWM.screen_margin = 0
PaperWM.window_gap = 0
PaperWM.swipe_fingers = 3

---- keybinds ----
hs.hotkey.bind({"cmd"}, "return", function()
    local app = hs.application.launchOrFocus("ghostty")
end)

hs.hotkey.bind({"cmd"}, "e", function()
    local app = hs.application.launchOrFocus("emacs")
end)

hs.hotkey.bind({"cmd", "shift"}, "b", function()
    local app = hs.application.launchOrFocus("librewolf")
end)

PaperWM:bindHotkeys({
	  -- switch to a new focused window in tiled grid
	  focus_left  = {{"alt", "cmd"}, "left"},
	  focus_right = {{"alt", "cmd"}, "right"},
	  focus_up    = {{"alt", "cmd"}, "up"},
	  focus_down  = {{"alt", "cmd"}, "down"},

	  -- switch windows by cycling forward/backward
	  -- (forward = down or right, backward = up or left)
	  focus_prev = {{"alt", "cmd"}, "k"},
	  focus_next = {{"alt", "cmd"}, "j"},

	  -- move windows around in tiled grid
	  swap_left  = {{"alt", "cmd", "shift"}, "left"},
	  swap_right = {{"alt", "cmd", "shift"}, "right"},
	  swap_up    = {{"alt", "cmd", "shift"}, "up"},
	  swap_down  = {{"alt", "cmd", "shift"}, "down"},

	  -- position and resize focused window
	  full_width           = {{"alt", "cmd"}, "m"},
	  cycle_width          = {{"alt", "cmd"}, "r"},
	  reverse_cycle_width  = {{"ctrl", "alt", "cmd"}, "r"},
	  cycle_height         = {{"alt", "cmd", "shift"}, "r"},
	  reverse_cycle_height = {{"ctrl", "alt", "cmd", "shift"}, "r"},

	  -- increase/decrease width
	  increase_width = {{"alt", "cmd"}, "l"},
	  decrease_width = {{"alt", "cmd"}, "h"},

	  -- move focused window into / out of a column
	  slurp_in = {{"alt", "cmd"}, "i"},
	  barf_out = {{"alt", "cmd"}, "o"},

	  -- move the focused window into / out of the tiling layer
	  toggle_floating = {{"alt", "cmd", "shift"}, "f"},

	  -- focus the first / second / etc window in the current space
	  focus_window_1 = {{"cmd", "shift"}, "1"},
	  focus_window_2 = {{"cmd", "shift"}, "2"},
	  focus_window_3 = {{"cmd", "shift"}, "3"},
	  focus_window_4 = {{"cmd", "shift"}, "4"},
	  focus_window_5 = {{"cmd", "shift"}, "5"},
	  focus_window_6 = {{"cmd", "shift"}, "6"},
	  focus_window_7 = {{"cmd", "shift"}, "7"},
	  focus_window_8 = {{"cmd", "shift"}, "8"},
	  focus_window_9 = {{"cmd", "shift"}, "9"},

	  -- switch to a new Mission Control space
	  switch_space_l = {{"alt", "cmd"}, ","},
	  switch_space_r = {{"alt", "cmd"}, "."},
	  switch_space_1 = {{"alt", "cmd"}, "1"},
	  switch_space_2 = {{"alt", "cmd"}, "2"},
	  switch_space_3 = {{"alt", "cmd"}, "3"},
	  switch_space_4 = {{"alt", "cmd"}, "4"},
	  switch_space_5 = {{"alt", "cmd"}, "5"},
	  switch_space_6 = {{"alt", "cmd"}, "6"},
	  switch_space_7 = {{"alt", "cmd"}, "7"},
	  switch_space_8 = {{"alt", "cmd"}, "8"},
	  switch_space_9 = {{"alt", "cmd"}, "9"},

	  -- move focused window to a new space and tile
	  move_window_1 = {{"alt", "cmd", "shift"}, "1"},
	  move_window_2 = {{"alt", "cmd", "shift"}, "2"},
	  move_window_3 = {{"alt", "cmd", "shift"}, "3"},
	  move_window_4 = {{"alt", "cmd", "shift"}, "4"},
	  move_window_5 = {{"alt", "cmd", "shift"}, "5"},
	  move_window_6 = {{"alt", "cmd", "shift"}, "6"},
	  move_window_7 = {{"alt", "cmd", "shift"}, "7"},
	  move_window_8 = {{"alt", "cmd", "shift"}, "8"},
	  move_window_9 = {{"alt", "cmd", "shift"}, "9"}
})

---- start spoons ----
PaperWM:start()
WarpMouse:start()
