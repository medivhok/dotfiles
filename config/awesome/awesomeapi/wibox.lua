---------------------------------------------------------------------------------------------------
-- @classmod awesomeapi.wibox
-- @author Jean Gregory Verret <gregory.verret@gmail.com>
-- @copyright 2018 Jean Gregory Verret
-- @license MIT
---------------------------------------------------------------------------------------------------

-- globals to locals
local setmetatable = setmetatable

-- from now on, no globals
_ENV = nil


local wibox = { mt = {} }


---
-- @function awesomeapi.wibox
function wibox:init(wiboxlib)
   self.mt.__index = assert(wiboxlib, 'argument \'wiboxlib\' is missing.')

   return setmetatable(self, self.mt)
end


function wibox.mt:__call(...)
   return wibox:init(...)
end


return setmetatable(wibox, wibox.mt)
