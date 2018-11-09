---------------------------------------------------------------------------------------------------
-- @classmod AwesomeApi
-- @author Jean Gregory Verret
-- @copyright 2018 Jean Gregory Verret
-- @license MIT
---------------------------------------------------------------------------------------------------

-- Submodule(s)
local Beautiful = require(... .. '.wrappers.Beautiful')
local GShape    = require(... .. '.GShape')
local utils     = require(... .. '.utils')

-- Globals to locals
local assert       = assert
local error        = error
local ipairs       = ipairs
local loadfile     = loadfile
local os           = os
local pcall        = pcall
local setmetatable = setmetatable
local tostring     = tostring
local type         = type

-- from now on, no globals
_ENV = nil

local AwesomeApi = {
    -- Default configurations
    browser   = 'firefox',
    editor    = os.getenv('EDITOR') or 'vim',
    modkey    = 'Mod4',
    terminal  = 'xterm',

    mt = {},
}


---
function AwesomeApi:editorCmd()
    return self.terminal .. ' -e ' .. self.editor
end


-- ------------------------------------------------------------------------------------------------
-- Shortcuts methods
-- ------------------------------------------------------------------------------------------------

---
-- @see awesome.beautiful.xresources.apply_dpi
function AwesomeApi:dpi(...)
    return self.beautiful.xresources.apply_dpi(...)
end


-- ------------------------------------------------------------------------------------------------
-- Notifications (naughty.notify)
-- ------------------------------------------------------------------------------------------------
function AwesomeApi:message(text, title)
    title = title or '[Message] AwesomeApi'

    self.naughty.notify({
        preset = self.notifyPresets.normal,
        title = title,
        text = text,
    })

    return self
end


function AwesomeApi:error(text, title)
    title = title or '[Error] AwesomeApi'

    self.naughty.notify({
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


-- ------------------------------------------------------------------------------------------------
-- Initialization and configuration methods
-- ------------------------------------------------------------------------------------------------

---
function AwesomeApi:setTheme(options)
    options = options or {}

    self.beautiful:configure({
        themeName = options.name,
        screensOrder = options.screens,
    })

    self.gears:configure({
        roundedRectRadius = options.roundedRectRadius,
    })

    return self
end


---
function AwesomeApi:configure(options)
    options = options or {}
    local config = {}

    for _, filename in ipairs(self.configFiles) do
        local _, result = pcall(loadfile, filename)

        if type(result) == 'function' then
            config = utils.mergeTables(result(), config)
        end
    end

    config = utils.mergeTables(options, config)

    self.browser  = config.browser or self.browser
    self.editor   = config.editor or self.editor
    self.modkey   = config.modkey or self.modkey
    self.terminal = config.terminal or self.terminal

    self.gshape:configure(config.gshape)

    return self
end


---
-- @param env The global environment in awesomewm.
-- @param env.awesome The awesome global object.
-- @param env.client The client global object.
-- @param env.screen The screen global object.
-- @param env.root The root global object.
-- @return The initialized @{AwesomeApi} object.
function AwesomeApi:init(awesomeEnv, awesomeLibs, options)
    -- first we validate the arguments
    awesomeEnv  = awesomeEnv or {}
    assert(awesomeEnv.awesome, 'The \'awesomeEnv.awesome\' argument is mandatory.')
    assert(awesomeEnv.client, 'The \'awesomeEnv.client\' argument is mandatory.')
    assert(awesomeEnv.screen, 'The \'awesomeEnv.screen\' argument is mandatory.')
    assert(awesomeEnv.root, 'The \'awesomeEnv.root\' argument is mandatory.')

    awesomeLibs = awesomeLibs or {}
    assert(awesomeLibs.awful, 'The \'awesomeLibs.awful\' argument is mandatory.')
    assert(awesomeLibs.gears, 'The \'awesomeLibs.gears\' argument is mandatory.')

    self.mt.__index = awesomeEnv.awesome
    setmetatable(self, self.mt)

    -- globals
    self.clients    = awesomeEnv.client
    self.screens    = awesomeEnv.screen
    self.rootWindow = awesomeEnv.root

    -- modules
    self.awful        = assert(awesomeLibs.awful, 'The \'awesomeLibs.awful\' argument is mandatory.')
    self.beautiful    = Beautiful(beautiful)
    self.gears        = Gears(gears)
    self.hotkeysPopup = hotkeysPopup
    self.naughty      = naughty
    self.wibox        = wibox

    self.gfs = gears.filesystem
    self.gshape = GShape(gears.shape)

    -- Properties
    self.notifyPresets = naughty.config.presets


    self.configFiles = {
        [1] = self.gfs.get_configuration_dir() .. 'apiconfig.lua',
        [2] = self.gfs.get_xdg_config_home() .. 'awesome-api/config.lua',
    }

    -- then we check if awesome encountered an error during startup
    if self.startup_errors then
        awesomeApi:error('Oops, there were errors during startup!', startup_errors)
    end

    -- Handle runtime errors after startup
    self.connect_signal('debug::error', self:onError())

    return self
end


---
-- @function AwesomeApi
--
function AwesomeApi.mt:__call(...)
    return AwesomeApi:init(...)
end


return setmetatable(AwesomeApi, AwesomeApi.mt)
