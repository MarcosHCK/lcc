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

--- @class ItemLR0Rule: { [1]: NonTerminalSymbol, [2]: integer, [3]: integer, [4]: boolean }
--- @class ItemLR1Rule: { [1]: NonTerminalSymbol, [2]: integer, [3]: integer, [4]: boolean, [5]: TerminalSymbol }

local format = string.format

do
  ---
  --- @param linesof LinesOf
  --- @return Item
  ---
  function constructor (linesof)

    --- @class Item
    --- @field private store table<string, ItemLR1Rule>
    local Item = {}

    local item_mt, rule_mt, serialize

    item_mt =
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

    rule_mt =
      {
        __eq = function (self, other)

          local eq_base = self [1] == other [1]
          local eq_nprod = self [2] == other [2]
          local eq_at = self [3] == other [3]
          local eq_lookahead = self [5] == other [5]

          return eq_base and eq_nprod and eq_at and eq_lookahead
        end,

        __hash = function (self) return serialize (self) end,

        __name = 'ItemRule',

        __tostring = function (self)

          local kernel = self [4]
          local state = not kernel and '' or '*'

          return ('%s%s'):format (serialize (self), state)
        end
      }

    ---
    --- @param self ItemLR0Rule | ItemLR1Rule
    --- @return string
    ---
    function serialize (self)

      local base = self [1]
      local nprod = self [2]
      local at = self [3]
      local lookahead = self [5]

      local meta = getmetatable (base).__tostring
      local tostr = function (e) return meta (e, true) end

      local look = not lookahead and '' or ', ' .. tostr (lookahead)
      local prods = List.map (linesof [base] [nprod], tostr)
      local prod = table.concat (List.insert (prods, at, '•'), ' ')

      return ('[%s -> %s%s]'):format (tostr (base), prod, look)
    end

    ---
    --- @param rule ItemLR1Rule
    --- @return self
    --- @return boolean was_added
    ---
    function Item:add (rule)

      utils.assert_arg (0, self, 'table', Item.is, 'not an Item')
      utils.assert_arg (1, rule, 'table', Item.isRule, 'not an ItemRule')
      local key = serialize (rule)

      if (self.store [key] ~= nil) then

        return self, false
      else

        OrderedMap.set (self.store, key, rule)
        return self, true
      end
    end

    --- @type fun(arg: any): boolean
    function Item.is (arg) return utils.is_type (arg, item_mt) end

    --- @type fun(arg: any): boolean
    function Item.isRule (arg) return utils.is_type (arg, rule_mt) end

    ---
    --- @return fun(): integer?, ItemLR1Rule
    ---
    function Item:iter ()

      utils.assert_arg (0, self, 'table', Item.is, 'not an Item')
      local i, iter = 0, OrderedMap.iter (self.store)

      return function ()

        local v
        i, _, v = i + 1, iter ()

        --- @cast v ItemLR1Rule
        return v ~= nil and i or nil, v
      end
    end

    --- @param list? List<ItemLR1Rule>
    --- @return Item
    ---
    function Item.new (list)

      local store = OrderedMap { }

      for i, elm in ipairs (list or {}) do

        utils.assert_arg (1, elm, 'table', Item.isRule, format ('not an ItemRule (index %i)', i))
        OrderedMap.set (store, serialize (elm), elm)
      end
    return setmetatable ({ store = store }, item_mt)
    end

    --- @param base NonTerminalSymbol
    --- @param nprod integer
    --- @param at integer
    --- @param kernel? boolean
    --- @return ItemLR0Rule
    ---
    function Item.rulelr0 (base, nprod, at, kernel)

      utils.assert_arg (1, base, 'table')
      utils.assert_arg (2, nprod, 'number', function (n) return n == math.floor (n) end, 'not an integer')
      utils.assert_arg (3, at, 'number', function (n) return n == math.floor (n) end, 'not an integer')
      kernel = kernel == nil and false or utils.assert_arg (4, kernel, 'boolean')

      return setmetatable ({ base, nprod, at, kernel == true }, rule_mt)
    end

    --- @param base NonTerminalSymbol
    --- @param nprod integer
    --- @param at integer
    --- @param kernel boolean
    --- @param terminal TerminalSymbol
    --- @return ItemLR0Rule
    ---
    function Item.rulelr1 (base, nprod, at, kernel, terminal)

      utils.assert_arg (1, base, 'table')
      utils.assert_arg (5, terminal, 'table')
      utils.assert_arg (2, nprod, 'number', function (n) return n == math.floor (n) end, 'not an integer')
      utils.assert_arg (3, at, 'number', function (n) return n == math.floor (n) end, 'not an integer')
      kernel = kernel == nil and false or utils.assert_arg (4, kernel, 'boolean')

      return setmetatable ({ base, nprod, at, kernel == true, terminal }, rule_mt)
    end

    --- @type fun(self: Item): integer
    function Item.size (self) return List.len (OrderedMap.keys (self.store)) end

    return Item
  end
return constructor
end
