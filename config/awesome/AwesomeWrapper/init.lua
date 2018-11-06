---------------------------------------------------------------------------------------------------
-- @classmod AwesomeWrapper
-- @author Jean Gregory Verret
-- @copyright 2018 Jean Gregory Verret
-- @license MIT
---------------------------------------------------------------------------------------------------
local GearsWrapper = require('AwesomeWrapper.GearsWrapper')

local setmetatable = setmetatable

-- from now on, no globals
_ENV = nil


local AwesomeWrapper = { mt = {} }


---
function AwesomeWrapper:initialize(args)
    self.clients      = args.client
    self.screens      = args.screen
    self.rootWindow   = args.root
    self.awful        = args.awful
    self.beautiful    = args.beautiful
    self.gears        = GearsWrapper(args.gears)
    self.hotkeysPopup = args.hotkeysPopup
    self.naughty      = args.naughty
    self.wibox        = args.wibox

    self.mt.__index = args.awesome

    return setmetatable(self, self.mt)
end


---
-- @functions mac.AwesomeWrapper
--
return function(args)
    return AwesomeWrapper:initialize(args)
end
