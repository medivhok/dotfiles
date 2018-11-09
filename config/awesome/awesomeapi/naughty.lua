---------------------------------------------------------------------------------------------------
-- @classmod awesomeapi.naughty
-- @author Jean Gregory Verret <gregory.verret@gmail.com>
-- @copyright 2018 Jean Gregory Verret
-- @license MIT
---------------------------------------------------------------------------------------------------

-- globals to locals
local setmetatable = setmetatable

-- from now on, no globals
_ENV = nil


local naughty = { mt = {} }


---
-- @function awesomeapi.naughty
function naughty:init(naughtylib)
   self.mt.__index = assert(naughtylib, 'argument \'naughtylib\' is missing.')

   return setmetatable(self, self.mt)
end


function naughty.mt:__call(...)
   return naughty:init(...)
end


return setmetatable(naughty, naughty.mt)
