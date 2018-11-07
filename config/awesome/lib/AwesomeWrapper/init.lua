---------------------------------------------------------------------------------------------------
-- @classmod AwesomeWrapper
-- @author Jean Gregory Verret
-- @copyright 2018 Jean Gregory Verret
-- @license MIT
---------------------------------------------------------------------------------------------------
-- Awesome module(s)
local awful        = require('awful')
local beautiful    = require('beautiful')
local dpi          = require("beautiful.xresources").apply_dpi
local gears        = require('gears')
local hotkeysPopup = require('awful.hotkeys_popup').widget
local naughty      = require('naughty')
local wibox        = require('wibox')

-- Project's module(s)
local BeautifulWrapper = require('AwesomeWrapper.BeautifulWrapper')
local GearsWrapper = require('AwesomeWrapper.GearsWrapper')

-- Globals to locals
local setmetatable = setmetatable

-- from now on, no globals
_ENV = nil


local AwesomeWrapper = { mt = {} }


function AwesomeWrapper:onError()
    local inError = false

    return function(err)
        if inError == false then
            inError = true
            self.notify({
                preset = self.naughtyPresets.critical,
                title = 'Oops, an error happened!',
                text = tostring(err),
            })
            inError = false
        end
    end
end

---
function AwesomeWrapper:new(env, configs)
    -- globals
    self.clients      = env.client
    self.screens      = env.screen
    self.rootWindow   = env.root

    -- modules
    self.awful        = awful
    self.beautiful    = BeautifulWrapper(beautiful, configs)
    self.gears        = GearsWrapper(gears)
    self.hotkeysPopup = hotkeysPopup
    self.naughty      = naughty
    self.wibox        = wibox

    -- module shortcuts
    self.dpi = dpi
    self.notify = naughty.notify
    self.notifyPresets = naughty.config.presets

    self.mt.__index = env.awesome


    -- Check if awesome encountered an error during startup and fell back to
    -- another config (This code will only ever execute for the fallback config)
    if self.startup_errors then
        self.notify({
            preset = self.notifyPresets.critical,
            title = 'Oops, there were errors during startup!',
            text = self.startup_errors,
        })
    end


    -- Handle runtime errors after startup
    self.connect_signal('debug::error', self:onError())

    return setmetatable(self, self.mt)
end


---
-- @functions mac.AwesomeWrapper
--
function AwesomeWrapper.mt:__call(...)
    return AwesomeWrapper:new(...)
end


return setmetatable(AwesomeWrapper, AwesomeWrapper.mt)
