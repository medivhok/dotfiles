---------------------------------------------------------------------------------------------------
-- @classmod AwesomeApi.wrappers.Beautiful
-- @author Jean Gregory Verret <gregory.verret@gmail.com>
-- @copyright 2018 Jean Gregory Verret
-- @license MIT
---------------------------------------------------------------------------------------------------

-- Globals to locals
local assert = assert
local setmetatable = setmetatable

-- from now on, no globals
_ENV = nil


-- the class
local Beautiful = { _mt = {} }


function Beautiful:configure(config)
    config = config or {}
    return self
end


---
-- @function mac.wrappers.BeautifulWrapper
-- @param beautiful The Awesome beautiful module
-- @return A @{BeautifulWrapper} instance
--
function Beautiful:initialize(beautiful)
    if not self._super then
        self._super = assert(beautiful, 'The \'beautiful\' argument is mandatory.')
        self._mt.__index = beautiful

        setmetatable(self, self._mt)
    end

    return self
end


---
-- @function wrappers.Beautiful
--
return function(...)
    return Beautiful:initialize(...)
end
