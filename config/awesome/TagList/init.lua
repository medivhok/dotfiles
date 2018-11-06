local TagListButtons = require('TagList.TagListButtons')

local setmetatable = setmetatable;

-- no globals from this point
_ENV = nil


local TagList = { mt = {} }


function TagList:new(screen, awesomeApi, options)
   return awesomeApi.awful.widget.taglist {
      screen = screen,
      filter = awesomeApi.awful.widget.taglist.filter.all,
      buttons = TagListButtons(awesomeApi, options)
   }
end


function TagList.mt:__call(...)
   return TagList:new(...)
end


return setmetatable(TagList, TagList.mt)
