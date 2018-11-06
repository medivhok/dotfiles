---------------------------------------------------------------------------------------------------
-- @module mac.wrappers.BeautifulWrapper
-- @author Jean Gregory Verret
-- @copyright 2018 Jean Gregory Verret
---------------------------------------------------------------------------------------------------
local Colors = require('mac.Colors')
local themes = require('mac.themes')

local error = error
local type = type

-- from now on, no globals
_ENV = nil


-- the class
local BeautifulWrapper = { mt = {} }


---
-- @function mac.wrappers.BeautifulWrapper
-- @param beautiful The Awesome beautiful module
-- @return A @{Beautiful} object
--
function BeautifulWrapper:set(beautiful)
    if type(beautiful) ~= 'table' then
        error('the \'beautiful\' argument is mandatory and mus be a table.')
    end

    self._super = beautiful
    self.mt.__index = beautiful

    return setmetatable(self, self.mt)
end


return function(beautiful)
    return BeautifulWrapper:setBeautiful(beautiful)
end
