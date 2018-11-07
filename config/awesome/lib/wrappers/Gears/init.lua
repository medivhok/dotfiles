---------------------------------------------------------------------------------------------------
-- @classmod wrappers.Gears
-- @author Jean Gregory Verret <gregory.verret@gmail.com>
-- @copyright 2018 Jean Gregory Verret
-- @license MIT
---------------------------------------------------------------------------------------------------
local Shape = require('wrappers.Gears.Shape')

-- Globals to locals
local assert = assert
local setmetatable = setmetatable

-- from now on, no globals
_ENV = nil


-- the class
local Gears = {}


---
-- @param gears The Awesome gears module
-- @return The @{GearsWrapper} instance
--
function Gears:initialize(gears)
    if not self._super then
        self._super = assert(gears, 'The \'gears\' argument is mandatory.')
        self.shape = Shape(gears.shape)

        setmetatable(self, { __index = self._super })
    end

    return self
end


---
-- @function AwesomeWrapper.GearsWrapper
return function(...)
    return Gears:initialize(...)
end
