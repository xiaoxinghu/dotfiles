hyper:bind({}, "return", function()
    local win = hs.window.frontmostWindow()
    win:setFullscreen(not win:isFullscreen())
    hyper.triggered = true
end)

local ratio = 3

hyper:bind({}, "Left", function()
    local win = hs.window.focusedWindow()
    local f = win:frame()
    local screen = win:screen()
    local max = screen:frame()
    local width = math.max(600, max.w / ratio)
    f.x = max.x
    f.y = max.y
    f.w = width
    f.h = max.h
    win:setFrame(f)
end)

hyper:bind({}, "Right", function()
    local win = hs.window.focusedWindow()
    local f = win:frame()
    local screen = win:screen()
    local max = screen:frame()
    local width = math.max(600, max.w / ratio)
    f.x = max.w - width
    f.y = max.y
    f.w = width
    f.h = max.h
    win:setFrame(f)
end)
