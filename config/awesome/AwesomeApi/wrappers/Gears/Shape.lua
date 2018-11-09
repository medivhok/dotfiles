---------------------------------------------------------------------------------------------------
-- @classmod AwesomeApi.wrappers.Gears.Shape
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


-- Default values
local Defaults = {
    roundedRectRadius = 10,
}


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

        -- Insert default values
        for k, v in pairs(Defaults) do
            self[k] = v
        end

        setmetatable(self, {
            __index = shape,
            __newindex = function(table, key, value)
                if table[key] and value == nil and Defaults[key] ~= nil then
                    table[key] = Defaults[key]
                else
                    table[key] = value
                end
            end,
        })
    end

    return self
end


---
-- @function wrappers.Gears.Shape
return function(...)
    return Shape:initialize(...)
end
