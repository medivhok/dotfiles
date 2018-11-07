---------------------------------------------------------------------------------------------------
-- @classmod AwesomeWrapper.BeautifulWrapper
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
local BeautifulWrapper = { _mt = {} }

---
function BeautifulWrapper.init(theme)
    return BeautifulWrapper._super.init(theme)
end


function BeautifulWrapper:setTheme(theme)
    return self
end


---
-- @function mac.wrappers.BeautifulWrapper
-- @param beautiful The Awesome beautiful module
-- @return A @{BeautifulWrapper} instance
--
function BeautifulWrapper:initialize(beautiful)
    self._super = assert(beautiful, 'The \'beautiful\' argument is mandatory.')
    self._wrapped = true;
    self._mt.__index = beautiful

    -- reset the BeautifulMetatable
    setmetatable(self, self._mt)
    self.__index = self

    return setmetatable({ _super = self }, self)
end


function BeautifulWrapper._mt:__call(...)
    return BeautifulWrapper:initialize(...)
end


return setmetatable(BeautifulWrapper, BeautifulWrapper._mt)
