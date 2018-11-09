---------------------------------------------------------------------------------------------------
-- @classmod awesomeapi.beautiful
-- @author Jean Gregory Verret <gregory.verret@gmail.com>
-- @copyright 2018 Jean Gregory Verret
-- @license MIT
---------------------------------------------------------------------------------------------------

-- globals to locals
local setmetatable = setmetatable

-- from now on, no globals
_ENV = nil


local beautiful = { mt = {} }


---
-- @function awesomeapi.beautiful
function beautiful:init(beautifullib)
   self.mt.__index = assert(beautifullib, 'argument \'beautifullib\' is missing.')

   return setmetatable(self, self.mt)
end


function beautiful.mt:__call(...)
   return beautiful:init(...)
end


return setmetatable(beautiful, beautiful.mt)
