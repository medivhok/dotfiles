---------------------------------------------------------------------------------------------------
-- @classmod AwesomeApi
-- @author Jean Gregory Verret
-- @copyright 2018 Jean Gregory Verret
-- @license MIT
---------------------------------------------------------------------------------------------------
-- Awesome module(s)
local awful        = require('awful')
local beautiful    = require('beautiful')
local bXResources  = require("beautiful.xresources")
local gears        = require('gears')
local hotkeysPopup = require('awful.hotkeys_popup').widget
local naughty      = require('naughty')
local wibox        = require('wibox')

-- Project's module(s)
local Beautiful = require('wrappers.Beautiful')
local Gears     = require('wrappers.Gears')

-- Globals to locals
local assert       = assert
local ipairs       = ipairs
local loadfile     = loadfile
local pcall        = pcall
local setmetatable = setmetatable
local tostring     = tostring

-- from now on, no globals
_ENV = nil


local AwesomeApi = {}


function AwesomeApi:message(text, title)
    title = title or 'Message!'

    self.notify({
        preset = self.notifyPresets.normal,
        title = title,
        text = text,
    })

    return self
end


function AwesomeApi:error(text, title)
    title = title or 'Error!'

    self.notify({
        preset = self.notifyPresets.critical,
        title = title,
        text = text,
    })

    return self
end


-- ------------------------------------------------------------------------------------------------
-- Event handlers
-- ------------------------------------------------------------------------------------------------
function AwesomeApi:onError()
    local inError = false

    return function(err)
        if inError == false then
            inError = true
            self:error('Oops, an error happened!', tostring(err))
            inError = false
        end
    end
end


--
local function loadConfigFile(filename)
    local config = {}
    local status, result = pcall(loadfile, filename)

    if status == true then
        config = result()
    end

    return config
end


-- ------------------------------------------------------------------------------------------------
-- Some helper methods
-- ------------------------------------------------------------------------------------------------
function AwesomeApi:dpi()
    return bXResources.apply_dpi
end


function AwesomeApi:gfs()
    return self.gears.filesystem
end


---
function AwesomeApi:configure(config)
    local crush = self.gears.table.crush
    local finalConfig = {}

    for index, value in ipairs(self.configFiles) do
        crush(finalConfig, loadConfigFile(value))
    end

    if config then
        crush(finalConfig, config)
    end

    return config
end


---
-- @param env The global environment in awesomewm.
-- @param env.awesome The awesome global object.
-- @param env.client The client global object.
-- @param env.screen The screen global object.
-- @param env.root The root global object.
-- @return An initialized @{AwesomeApi} object.
function AwesomeApi:initialize(env)
    if not self._super then
        -- globals
        self._super     = assert(env.awesome, 'The \'env.awesome\' argument is mandatory.')
        self.clients    = assert(env.client, 'The \'env.client\' argument is mandatory.')
        self.screens    = assert(env.screen, 'The \'env.screen\' argument is mandatory.')
        self.rootWindow = assert(env.root, 'The \'env.root\' argument is mandatory.')

        -- modules
        self.awful        = awful
        self.beautiful    = Beautiful(beautiful)
        self.gears        = Gears(gears)
        self.hotkeysPopup = hotkeysPopup
        self.naughty      = naughty
        self.wibox        = wibox

        setmetatable(self, { __index = self._super })

        -- Properties
        self.configFiles = {
            [1] = self:gfs().get_configuration_dir() .. '/config.lua',
            [2] = self:gfs().get_xdg_config_home() .. '/awesome-api/config.lua',
        }

        self.notifyPresets = naughty.config.presets


        -- Check if awesome encountered an error during startup and fell back to
        -- another config (This code will only ever execute for the fallback config)
        if self.startup_errors then
            self:error('Oops, there were errors during startup!', self.startup_errors)
        end

        -- Handle runtime errors after startup
        self.connect_signal('debug::error', self:onError())
    end

    return self
end


---
-- @function AwesomeApi
--
return function(env)
    return AwesomeApi:initialize(env)
end
