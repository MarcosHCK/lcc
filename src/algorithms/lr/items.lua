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
local List  = require ('pl.List')
local Map   = require ('pl.Map')
local OrderedMap = require ('pl.OrderedMap')
local utils = require ('pl.utils')

--- @module "algorithms.lr.item"

--- @type SetConstructor<Closure>
local constructor

local format = string.format

do
  ---
  --- @param Item Item
  --- @return Items
  ---
  function constructor (Item)

    local serialize

    --- @class Items: Item[]
    --- @field private backref table<string, integer>
    --- @field private store table<string, Item>
    local Items = { }

    local meta =
      {
        __hash = function (self) return '{' .. table.concat (OrderedMap.keys (self.store), ',') .. '}' end,
        __index = function (self, k) return Items[k] or self.store [OrderedMap.keys (self.store) [k]] end,
        __name = 'Items',
        __tostring = function (self) return tostring (OrderedMap.values (self.store)) end
      }

    --- @param item Item
    --- @return string
    ---
    function serialize (item) return getmetatable (item).__hash (item) end

    --- @param item Item
    --- @return self, boolean
    ---
    function Items:add (item)

      utils.assert_arg (0, self, 'table', Items.is, 'not an Items instance')
      utils.assert_arg (1, item, 'table', Item.is, 'not an Item instance')
      local key = serialize (item)

      if (self.store [key] ~= nil) then

        return self, false
      else

        Map.set (self.backref, key, Item.size (self) + 1)
        OrderedMap.set (self.store, key, item)
        return self, true
      end
    end

    --- @param item Item
    --- @return integer?
    ---
    function Items:index (item)

      utils.assert_arg (0, self, 'table', Items.is, 'not an Items instance')
      utils.assert_arg (1, item, 'table', Item.is, 'not an Item instance')
      return self.backref [serialize (item)]
    end

    --- @type fun(arg: any): boolean
    function Items.is (arg) return utils.is_type (arg, meta) end

    ---
    --- @return fun(): integer?, Item
    ---
    function Items:iter ()

      utils.assert_arg (0, self, 'table', Items.is, 'not an Items instance')
      local i, iter = 0, OrderedMap.iter (self.store)

      return function ()

        local v
        i, _, v = i + 1, iter ()

        --- @cast v Item
        return v ~= nil and i or nil, v
      end
    end

    ---
    --- @param list List<Item>
    --- @return Items
    ---
    function Items.new (list)

      local backref = Map { }
      local store = OrderedMap { }

      local self = setmetatable ({ backref = backref, store = store }, meta)

      for i, elm in ipairs (list) do

        utils.assert_arg (1, elm, 'table', Item.is, format ('not an Item (index %i)', i))
        Items.add (self, elm)
      end
      return self
    end

    function Items:size () return List.len (OrderedMap.keys (self.store)) end
  return Items
  end
return constructor
end
