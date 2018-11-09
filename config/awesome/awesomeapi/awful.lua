---------------------------------------------------------------------------------------------------
-- @classmod awesomeapi.awful
-- @author Jean Gregory Verret <gregory.verret@gmail.com>
-- @copyright 2018 Jean Gregory Verret
-- @license MIT
---------------------------------------------------------------------------------------------------

-- globals to locals
local setmetatable = setmetatable

-- from now on, no globals
_ENV = nil


local awful = { mt = {} }


---
-- @function awesomeapi.awful
function awful:init(awfullib)
   self.mt.__index = assert(awfullib, 'argument \'awfullib\' is missing.')

   return setmetatable(self, self.mt)
end


function awful.mt:__call(...)
   return awful:init(...)
end


return setmetatable(awful, awful.mt)
