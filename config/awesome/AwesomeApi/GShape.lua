---------------------------------------------------------------------------------------------------
-- @classmod AwesomeApi.GShape
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
local GShape = {
    roundedRectRadius = 10,
    mt = {},
}


---
function GShape:roundedRect()
    return function(cr, width, height)
        self.rounded_rect(cr, width, height, self.roundedRectRadius)
    end
end


---
function GShape:configure(options)
    options = options or {}

    self.roundedRectRadius = options.roundedRectRadius or self.roundedRectRadius

    return self
end


---
-- @param shape The Awesome gears.shape module.
-- @return The initialized @{GShape} object.
--
function GShape:init(shape)
    -- validate argument(s)
    self.mt.__index = assert(shape, 'The \'shape\' argument is mandatory.')
    setmetatable(self, self.mt)

    return self
end


---
-- @function AwesomeApi.GShape
function GShape.mt:__call(...)
    return GShape:init(...)
end


return setmetatable(GShape, GShape.mt)
