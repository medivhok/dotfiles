---------------------------------------------------------------------------------------------------
-- @module awesomeapi.gshape
-- @author Jean Gregory Verret <gregory.verret@gmail.com>
-- @copyright 2018 Jean Gregory Verret
-- @license MIT
---------------------------------------------------------------------------------------------------

-- Globals to locals
local setmetatable = setmetatable

-- from now on, no globals
_ENV = nil


local gshape = { mt = {} }


---
---
function gshape.rounded_rect_callback(shape)
    return function(radius)
        return function(cr, width, height)
            shape.rounded_rect(cr, width, height, radius)
        end
    end
end


---
-- @function awesomeapi.gshape
function gshape.mt:__call(shape)
    return {
        rounded_rect = gshape.rounded_rect_callback(shape),
    }
end


return setmetatable(gshape, gshape.mt)
