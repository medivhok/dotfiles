---------------------------------------------------------------------------------------------------
-- @classmod TagList.TagListButtons
-- @author Jean Gregory Verret
-- @copyright 2018 Jean Gregory Verret
-- @license MIT
---------------------------------------------------------------------------------------------------
local setmetatable = setmetatable
local table        = table

-- no globals from this point
_ENV = nil


local Button = {
    Left   = 1,
    Right  = 2,
    Middle = 3,
}

local function onLeftClick(tag)
    if type(tag.onClick) == 'function' then
        tag.onClick()
    else
        tag:view_only()
    end
end


local function onLeftClickWithModKey(clients)
    return function(tag)
        if clients.focus then
            clients.focus:move_to_tag(tag)
        end
    end
end

local function onMiddleClickWithModKey(clients)
    return function(tag)
        if clients.focus then
            clients.focus:toggle_tag(tag)
        end
    end
end


local TagListButtons = { mt = {} }


function TagListButtons:new(awesomeApi, options)
    local buttons = {}
    local clients = awesomeApi.clients
    local awfulButton = awesomeApi.awful.button
    local awfulTag = awesomeApi.awful.tag
    local modkey  = options.modkey

    return awesomeApi.gears.table.join(
        awfulButton({}, Button.Left, function(tag) tag:view_only() end),
        awfulButton({ modkey }, Button.Left, onLeftClickWithModKey(clients)),
        awfulButton({}, Button.Middle, awfulTag.viewtoggle),
        awfulButton({ modkey }, Button.Middle, onMiddleClickWithModKey(clients)),
        awfulButton({}, 4, function(tag) awfulTag.viewnext(tag.screen) end),
        awfulButton({}, 5, function(tag) awfulTag.viewprev(tag.screen) end)
    )
end


function TagListButtons.mt:__call(...)
    return TagListButtons:new(...)
end


return setmetatable(TagListButtons, TagListButtons.mt)
