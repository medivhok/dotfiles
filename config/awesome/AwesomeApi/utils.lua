---------------------------------------------------------------------------------------------------
-- @module AwesomeApi.utils
-- @author Jean Gregory Verret
-- @copyright 2018 Jean Gregory Verret
-- @license MIT
---------------------------------------------------------------------------------------------------

-- Globals to locals
local assert = assert
local pairs  = pairs
local table  = table
local type   = type

-- no globals from this point
_ENV = nil


local utils = {}


function utils.cloneTable(source, deep)
   local clone = {}

   assert(type(source) == 'table', 'The \'source\' argument must be a table.')
   deep = type(deep) == 'boolean' and deep or false

   for key, value in pairs(source) do
      local keyType = type(key)

      if keyType == 'number' then
         table.insert(clone, value)
      elseif keyType == 'table' and deep then
         clone[key] = utils.cloneTable(value, deep)
      else
         clone[key] = value
      end
   end

   return clone
end


--- Recursively merge two tables.
-- Recursively merge the source table into the target table. The target fields will be overwritten
-- by the source fields, unless the field is a table. In such case, those fields will be merged.
-- @tparam {table} source The table from which the fields will be taken.
-- @tparam {table} target The table that will receive the fields.
-- @treturn {table} A new table.
--
function utils.mergeTables(source, target)
   assert(type(source) == 'table', 'The \'source\' argument must be a table.')
   assert(
      type(target) == 'table' or target == nil,
      'The \'target\' argument must be a table or nil.'
   )

   local ret = utils.cloneTable(target or {})

   for key, value in pairs(source) do
      if type(value) == 'table' then
         local targetValue = ret[key] or {}

         if type(targetValue) == 'table' then
            ret[key] = utils.mergeTables(targetValue, value)
         else
            error('The source contains a table field that is not a table in the target.')
         end
      elseif type(key) == 'number' then
         table.insert(ret, value)
      else
         ret[key] = value
      end
   end

   return ret
end


return utils
