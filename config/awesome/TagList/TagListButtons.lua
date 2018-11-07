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


local TagListButtons = { mt = {} }


function TagListButtons:buttons()
    return self._buttons
end


function TagListButtons:showTag()
    return function(tag)
        tag:view_only()
    end
end


function TagListButtons:moveFocusedClientToTag(clients)
    return function(tag)
        if clients.focus then
            clients.focus:move_to_tag(tag)
        end
    end
end


function TagListButtons:toggleFocusedClientTag(clients)
    return function(tag)
        if clients.focus then
            clients.focus:toggle_tag(tag)
        end
    end
end


function TagListButtons:new(awesomeApi, options)
    local awfulButton = awesomeApi.awful.button
    local awfulTag = awesomeApi.awful.tag
    local modkey  = options.modkey

    local tagListButtons = {}

    tagListButtons._buttons = awesomeApi.gears.table.join(
        awfulButton({}, Button.Left, self:showTag()),
        awfulButton({ modkey }, Button.Left, self:moveFocusedClientToTag(awesomeApi.clients)),
        awfulButton({}, Button.Middle, awfulTag.viewtoggle),
        awfulButton({ modkey }, Button.Middle, self:toggleFocusedClientTag(awesomeApi.clients)),
        awfulButton({}, 4, function(tag) awfulTag.viewnext(tag.screen) end),
        awfulButton({}, 5, function(tag) awfulTag.viewprev(tag.screen) end)
    )

    self.__index = self
    setmetatable(tagListButtons, self)

    return tagListButtons
end


function TagListButtons.mt:__call(...)
    return TagListButtons:new(...)
end


return setmetatable(TagListButtons, TagListButtons.mt)
