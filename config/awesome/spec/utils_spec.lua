require 'busted.runner'()

local utils = require('AwesomeApi.utils')


-- ------------------------------------------------------------------------------------------------
-- AwesomeApi.utils
-- ------------------------------------------------------------------------------------------------
describe('The \'AwesomeApi.utils\' module', function()
    it('should returns a table', function()
        assert.is_true(type(utils) == 'table')
    end)
end)


-- ------------------------------------------------------------------------------------------------
-- AwesomeApi.utils.cloneTable
-- ------------------------------------------------------------------------------------------------
describe('The \'AwesomeApi.utils.cloneTable\'', function()
    it('should be a function.', function()
          assert.is_true(type(utils.cloneTable) == 'function')
    end)

    it(
       'should throws if the source argument is not a table.',
       function()
          assert.has_error(function() utils.cloneTable('test') end)
       end
    )

    it('should returns a clone of the source.', function()
        local source = { test = 'Test' }
        local clone = utils.cloneTable(source)

        assert.are_not.equals(source, clone)
        assert.are.same(source, clone)
    end)
end)


-- ------------------------------------------------------------------------------------------------
-- AwesomeApi.utils.mergeTables
-- ------------------------------------------------------------------------------------------------
describe('The \'AwesomeApi.utils.mergeTables\'', function()
    it('should be a function.', function()
          assert.is_true(type(utils.mergeTables) == 'function')
    end)


    it(
       'should throws if the \'target\' argument is not a table or nil.',
       function()
          assert.has_error(function() utils.mergeTables({}, 4) end)
          assert.has_error(function() utils.mergeTables({}, 'test') end)
          assert.has_no.errors(function() utils.mergeTables({}) end)
          assert.has_no.errors(function() utils.mergeTables({}, nil) end)
          assert.has_no.errors(function() utils.mergeTables({}, {}) end)
       end
    )


    it(
       'should throws if the \'source\' argument is not a table.',
       function()
          assert.has_error(function() utils.mergeTables({}, 4) end)
          assert.has_error(function() utils.mergeTables('test', {}) end)
          assert.has_error(function() utils.mergeTables(nil, nil) end)
       end
    )


    it('should merge the target into the source.', function()
        local source = { test = 'test' }
        local result = utils.mergeTables(source, {})

        assert.are.same(source, result)
        assert.are_not.equals(source, result)
    end)
end)
