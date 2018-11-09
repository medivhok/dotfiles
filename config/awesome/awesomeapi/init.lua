---------------------------------------------------------------------------------------------------
-- @classmod awesomeapi
-- @author Jean Gregory Verret
-- @copyright 2018 Jean Gregory Verret
-- @license MIT
---------------------------------------------------------------------------------------------------

-- submodule(s)
local awful = require(... .. '.awful')
local beautiful = require(... .. '.beautiful')
local gears = require(... .. '.gears')
local utils = require(... .. '.utils')
local wibox = require(... .. '.wibox')

-- globals to locals
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

local awesomeapi = {
    --- The default browser to use.
    -- @tfield[opt=firefox] string browser The name of the browser command.
    browser = 'firefox',

    --- The default editor to use.
    -- @tfield[opt=vim] string editor The name of the editor command. Default to the 'EDITOR'
    -- environment variable if set at startup.
    editor = os.getenv('EDITOR') or 'vim',

    --- The modkey to use.
    -- @tfield[opt=Mod4] string The name of the modkey. Default is the 'Windows' key.
    modkey = 'Mod4',

    --- Default terminal to use.
    -- @tfield[opt=xterm] string terminal The name of the terminal command.
    terminal = 'xterm',

    --- The 'awful' module wrapper.
    -- @tfield[opt=nil] awesomeapi.awful awful Awesome's awful module.
    awful = nil,

    --- The 'beautiful' module wrapper.
    -- @tfield[opt=nil] awesomeapi.beautiful beautiful Awesome's beautiful module.
    beautiful = nil,

    --- The 'gears' module wrapper.
    -- @tfield[opt=nil] awesomeapi.gears gears Awesome's gears module.
    gears = nil,

    --- The 'naughty' module wrapper.
    -- @tfield[opt=nil] awesomeapi.naughty naughty Awesome's naughty module.
    naughty = nil,

    --- The 'wibox' module wrapper.
    -- @tfield[opt=nil] awesomeapi.wibox wibox Awesome's wibox module.
    wibox = nil,

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
function awesomeapi:dpi(...)
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
function awesomeapi:configure(options)
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
-- @tparam table env The global environment in awesomewm.
-- @param env.awesome The awesome global object.
-- @param env.client The client global object.
-- @param env.screen The screen global object.
-- @param env.root The root global object.
-- @tparam table lib The awesome librairies.
-- @tparam table options Configuration options.
-- @return The initialized @{awesomeapi} object.
function awesomeapi:init(env, lib, options)
    assert(type(env) == 'table', 'argument \'env\' must be a table.')

    self.mt.__index = assert(env.awesome, 'argument \'env.awesome\' is missing.')
    self.clients = assert(env.client, 'argument \'env.client\' is missing.')
    self.screens = assert(env.screen, 'argument \'env.screen\' is missing.')
    self.rootWindow = assert(env.root, 'argument \'env.awesome\' is missing.')

    assert(type(lib) == 'table', 'argument \'lib\' must be a table.')

    self.awful     = awful(lib.awful)
    self.beautiful = beautiful(lib.beautiful)
    self.gears     = gears(lib.gears)
    self.naughty   = naughty(lib.gears)
    self.wibox     = wibox(lib.wibox)

    setmetatable(self, self.mt)

    return self
end


---
-- @function awesomeapi
function awesomeapi.mt:__call(...)
    return awesomeapi:init(...)
end


return setmetatable(awesomeapi, awesomeapi.mt)
