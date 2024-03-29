-- Copyright 2021-2025 MarcosHCK
-- This file is part of lcc.
--
-- lcc is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.
--
-- lcc is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.
--
-- You should have received a copy of the GNU General Public License
-- along with lcc.  If not, see <http://www.gnu.org/licenses/>.
--
local List = require ('pl.List')
local OrderedMap = require ('pl.OrderedMap')
local utils = require ('pl.utils')

--- @type SetConstructor<Item>
local constructor

local format = string.format

do
  ---
  --- @param Rule Rule
  --- @param linesof LinesOf
  --- @return Item
  ---
  function constructor (Rule, linesof)

    --- @class Item
    --- @field private store table<string, Rule>
    local Item = {}

    local meta
    local serialize = Rule.serialize

    meta =
      {
        __eq = function (self, other)

          local len1 = List.len (OrderedMap.keys (self.store))
          local len2 = List.len (OrderedMap.keys (other.store))

          if (len1 ~= len2) then return false
          else for _, v in Item.iter (self) do

            if (not other.store [serialize (v)]) then return false
            end
          end return true end
        end,

        __hash = function (self) return '{' .. table.concat (OrderedMap.keys (self.store), ',') .. '}' end,

        __index = function (self, k) return Item[k] or self.store [OrderedMap.keys (self.store) [k]] end,
        __name = 'Item',

        __tostring = function (self) return tostring (OrderedMap.values (self.store)) end
      }

    ---
    --- @param rule Rule
    --- @return self
    --- @return boolean was_added
    ---
    function Item:add (rule)

      utils.assert_arg (0, self, 'table', Item.is, 'not an Item')
      utils.assert_arg (1, rule, 'table', Rule.is, 'not an ItemRule')
      local key = serialize (rule)

      if (self.store [key] ~= nil) then

        return self, false
      else

        OrderedMap.set (self.store, key, rule)
        return self, true
      end
    end

    --- @type fun(arg: any): boolean
    function Item.is (arg) return utils.is_type (arg, meta) end

    ---
    --- @return fun(): integer?, Rule
    ---
    function Item:iter ()

      utils.assert_arg (0, self, 'table', Item.is, 'not an Item')
      local i, iter = 0, OrderedMap.iter (self.store)

      return function ()

        local v
        i, _, v = i + 1, iter ()

        --- @cast v Rule
        return v ~= nil and i or nil, v
      end
    end

    --- @param list? List<Rule>
    --- @return Item
    ---
    function Item.new (list)

      local store = OrderedMap { }

      for i, elm in ipairs (list or {}) do

        utils.assert_arg (1, elm, 'table', Rule.is, format ('not an ItemRule (index %i)', i))
        OrderedMap.set (store, serialize (elm), elm)
      end
    return setmetatable ({ store = store }, meta)
    end

    --- @type fun(self: Item): integer
    function Item.size (self) return List.len (OrderedMap.keys (self.store)) end

    return Item
  end
return constructor
end
