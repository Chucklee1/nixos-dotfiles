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
