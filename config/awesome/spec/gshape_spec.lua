require 'busted.runner'()

local gshape = require('awesomeapi.gshape')


-- ------------------------------------------------------------------------------------------------
-- awesomeapi.gshape
-- ------------------------------------------------------------------------------------------------
describe('The awesomeapi.gshape module', function()
    it('should be a table.', function()
        assert.is_true(type(gshape) == 'table')
    end)


    it('should be callable.', function()
          assert.has_no.errors(function() gshape() end)
    end)
end)


-- ------------------------------------------------------------------------------------------------
-- awesomeapi.gshape.rounded_rect_callback
-- ------------------------------------------------------------------------------------------------
describe('awesomeapi.gshape.rounded_rect_callback', function()
    it('should be a function.', function()
          assert.is_true(type(gshape.rounded_rect_callback) == 'function')
    end)

    it('should returns a function.', function()
          local result = gshape.rounded_rect_callback()
          assert.is_true(type(result) == 'function')
    end)
end)


-- ------------------------------------------------------------------------------------------------
-- awesomeapi.gshape()
-- ------------------------------------------------------------------------------------------------
describe('awesomeapi.gshape()', function()
    it('should returns a table.', function()
        assert.is_true(type(gshape()) == 'table')
    end)


    it('should have a rounded_rect field.', function()
          assert.is.truthy(gshape().rounded_rect)
    end)
end)


-- ------------------------------------------------------------------------------------------------
-- awesomeapi.gshape().rounded_rect
-- ------------------------------------------------------------------------------------------------
describe('awesomeapi.gshape().rounded_rect', function()
    it('should be a function.', function()
        assert.is_true(type(gshape().rounded_rect) == 'function')
    end)


    it('should returns a function.', function()
        assert.is_true(type(gshape().rounded_rect()) == 'function')
    end)

    it('should call \'shape.rounded_rect\' with \'radius\'.', function()
        local cr       = 'test cr'
        local width    = 'test width'
        local height   = 'test height'
        local radius   = 'test radius'
        local shape    = {}

        shape.rounded_rect = spy.new(function() end)

        assert.has_no.errors(function()
            local callback = gshape(shape).rounded_rect(radius)
            callback(cr, width, height)
        end)

        assert.spy(shape.rounded_rect).was.called_with(cr, width, height, radius)
    end)
end)
