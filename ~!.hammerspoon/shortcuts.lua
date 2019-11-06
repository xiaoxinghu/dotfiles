hs.hotkey.bind({"cmd", "ctrl", "alt", "shift"}, "t", function()
    hs.application.launchOrFocus("iTerm")
end)

hs.hotkey.bind({"cmd", "ctrl", "alt", "shift"}, "e", function()
    hs.application.launchOrFocus("/Applications/Emacs.app")
end)

hs.hotkey.bind({"cmd", "ctrl", "alt", "shift"}, "b", function()
    hs.application.launchOrFocus("Safari")
end)

hs.hotkey.bind({"cmd", "ctrl", "alt", "shift"}, "x", function()
    hs.application.launchOrFocus("Xcode")
end)

hs.hotkey.bind({"cmd", "ctrl", "alt", "shift"}, "c", function()
    hs.urlevent.openURL("org-protocol://capture?template=t")
end)
