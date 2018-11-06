---------------------------------------------------------------------------------------------------
-- @classmod AwesomeWrapper.GearsWrapper
-- @author Jean Gregory Verret
-- @copyright 2018 Jean Gregory Verret
-- @license MIT
---------------------------------------------------------------------------------------------------
local ShapeWrapper = require('AwesomeWrapper.GearsWrapper.ShapeWrapper')

local error        = error
local setmetatable = setmetatable
local type         = type

-- from now on, no globals
_ENV = nil


-- the class
local GearsWrapper = { mt = {} }


---
-- @param gears The Awesome gears module
-- @return The @{GearsWrapper} instance
--
function GearsWrapper:new(gears)
    if type(gears) == 'table' then
        self._super = gears
        self.mt.__index = gears
        setmetatable(self, self.mt)

        if type(gears.shape) == 'table' then
            self.shape = ShapeWrapper(gears.shape)
        else
            self.shape = nil
        end
    else
        error('the \'gears\' argument must be a table.')
    end

    return self
end


---
-- @function AwesomeWrapper.GearsWrapper
function GearsWrapper.mt:__call(...)
    return GearsWrapper:new(...)
end


return setmetatable(GearsWrapper, GearsWrapper.mt)
