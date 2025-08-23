local clock = sbar.add("item", {
  label = {
    font = {
      size = 13,
    },
  },
  position = "right",
  update_freq = 1,
})

local function update()
  clock:set({ label = os.date("%m.%d.%Y | %H:%M:%S"), })
end

clock:subscribe("routine", update)
clock:subscribe("forced", update)
