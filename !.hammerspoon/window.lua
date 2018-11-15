hyper:bind({}, "return", function()
    local win = hs.window.frontmostWindow()
    win:setFullscreen(not win:isFullscreen())
    hyper.triggered = true
end)

function move(side, ratio)
    local win = hs.window.focusedWindow()
    local f = win:frame()
    local screen = win:screen()
    local max = screen:frame()
    local width = math.max(600, max.w / ratio)
    f.y = max.y
    f.w = width
    f.h = max.h
    if side == "left" then
       f.x = max.x
    else
       f.x = max.w - width
    end
    win:setFrame(f)
end

hyper:bind({}, "Left", function()
      move("left", 3)
end)

hyper:bind({}, "Right", function()
      move("right", 3)
end)

hyper:bind({}, "h", function()
      move("left", 3/2)
end)

hyper:bind({}, "l", function()
      move("right", 3/2)
end)
