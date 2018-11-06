---------------------------------------------------------------------------------------------------
-- @classmod AwesomeWrapper.GearsWrapper.ShapeWrapper
-- @author Jean Gregory Verret
-- @copyright 2018 Jean Gregory Verret
-- @licence MIT
---------------------------------------------------------------------------------------------------
local error        = error
local setmetatable = setmetatable
local type         = type

-- from now on, no globals
_ENV = nil


-- The class
local ShapeWrapper = {
    roundedRectRadius = 10,
    mt = {},
}


---
-- @param cr A cairo context
-- @tparam number width The shape width
-- @tparam number height The shape height
-- @raise If gearsShape is not set or does not contain a rounded_rect function
--
function ShapeWrapper:roundedRect(cr, width, height)
    self._super.rounded_rect(cr, width, height, self.roundedRectRadius)
end


---
-- @function mac.wrappers.wrap_gears_shape
-- @param gears The Awesome gears.shape module
-- @return The @{Shapes} object
--
function ShapeWrapper:new(shape)
    if type(shape) == 'table' then
        self._super = shape
        self.mt.__index = shape
        setmetatable(self, self.mt)
    else
        error('the \'shape\' argument must be a table.')
    end

    self.rounded_rect = function(...)
        return self:roundedRect(...)
    end

    return self
end


---
-- @function AwesomeWrapper.GearsWrapper.ShapeWrapper
function ShapeWrapper.mt:__call(...)
    return ShapeWrapper:new(...)
end


return setmetatable(ShapeWrapper, ShapeWrapper.mt)
