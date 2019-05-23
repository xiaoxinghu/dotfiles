hs.hotkey.bind({"cmd", "ctrl", "alt", "shift"}, "t", function()
    hs.application.launchOrFocus("iTerm")
end)

hs.hotkey.bind({"cmd", "ctrl", "alt", "shift"}, "e", function()
    hs.application.launchOrFocus("/Applications/Emacs.app")
end)

hs.hotkey.bind({"cmd", "ctrl", "alt", "shift"}, "b", function()
    hs.application.launchOrFocus("Safari")
end)
