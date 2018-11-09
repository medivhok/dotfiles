---------------------------------------------------------------------------------------------------
-- @classmod awesomeapi.gears
-- @author Jean Gregory Verret <gregory.verret@gmail.com>
-- @copyright 2018 Jean Gregory Verret
-- @license MIT
---------------------------------------------------------------------------------------------------

-- globals to locals
local setmetatable = setmetatable

-- from now on, no globals
_ENV = nil


local gears = { mt = {} }


---
-- @function awesomeapi.gears
function gears:init(gearslib)
   self.mt.__index = assert(gearslib, 'argument \'gearslib\' is missing.')

   return setmetatable(self, self.mt)
end


function gears.mt:__call(...)
   return gears:init(...)
end


return setmetatable(gears, gears.mt)
