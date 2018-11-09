---------------------------------------------------------------------------------------------------
-- @module awesomeapi.ui
-- @author Jean Gregory Verret <gregory.verret@gmail.com>
-- @copyright 2018 Jean Gregory Verret
-- @license MIT
---------------------------------------------------------------------------------------------------

-- Globals to locals
local assert       = assert
local pairs        = pairs
local setmetatable = setmetatable
local type         = type

-- from now on, no globals
_ENV = nil


-- The class
local ui = { mt = {} }


---
function ui.rounded_rect_callback(gshape)
    ---
    return function(radius)
        return function(cr, width, height)
            gshape.rounded_rect(cr, width, height, radius)
        end
    end
end


---
-- @param shape The Awesome gears.shape module.
-- @return The initialized @{GShape} object.
--
function ui:init(gshape)
    -- validate argument(s)
    assert(gshape, 'The \'shape\' argument is mandatory.')

    self.rounded_rect = rounded_rect_callback(gshape)

    return self
end


---
-- @function AwesomeApi.GShape
function ui.mt:__call(...)
    return ui:init(...)
end


return function(gshape)
    return
end
