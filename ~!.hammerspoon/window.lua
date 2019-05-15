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

function bind(key, side, ratio)
  hs.hotkey.bind({"cmd", "ctrl", "alt", "shift"}, key, function()
      move(side, ratio)
  end)
end

bind("Left", "left", 3)
bind("Right", "right", 3)
bind("h", "left", 3/2)
bind("l", "right", 3/2)
