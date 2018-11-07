---------------------------------------------------------------------------------------------------
-- @classmod wrappers.Gears.Shape
-- @author Jean Gregory Verret <gregory.verret@gmail.com>
-- @copyright 2018 Jean Gregory Verret
-- @licence MIT
---------------------------------------------------------------------------------------------------

-- Globals to locals
local assert       = assert
local setmetatable = setmetatable

-- from now on, no globals
_ENV = nil


-- The class
local Shape = {}


---
-- @param cr A cairo context
-- @tparam number width The shape width
-- @tparam number height The shape height
-- @raise If gearsShape is not set or does not contain a rounded_rect function
--
function Shape.rounded_rect(cr, width, height)
    Shape._super.rounded_rect(cr, width, height, Shape.roundedRectRadius)
end


---
-- @param gears The Awesome gears.shape module
-- @return The @{Shapes} object
--
function Shape:initialize(shape)
    if not self._super then
        self._super = assert(shape, 'The \'shape\' argument is mandatory.')

        self.roundedRectRadius = 10

        setmetatable(self, { __index = shape })
    end

    return self
end


---
-- @function wrappers.Gears.Shape
return function(...)
    return Shape:initialize(...)
end
