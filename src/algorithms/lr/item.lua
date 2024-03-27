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
local utils = require ('pl.utils')

--- @type SetConstructor<Item>
local constructor

--- @class ItemLR0: ItemLR0Rule[]
--- @class ItemLR1: ItemLR1Rule[]

--- @class ItemLR0Rule: { [1]: NonTerminalSymbol, [2]: integer, [3]: integer, [4]: boolean }
--- @class ItemLR1Rule: { [1]: NonTerminalSymbol, [2]: integer, [3]: integer, [4]: boolean, [5]: TerminalSymbol }

do

  --- @param linesof LinesOf
  --- @return Item
  ---
  function constructor (linesof)

    --- @class Item
    local Item = {}

    local rule_mt =
      {
        __eq = function (item1, item2)

          local eq_base = item1 [1] == item2 [1]
          local eq_nprod = item1 [2] == item2 [2]
          local eq_at = item1 [3] == item2 [3]
          local eq_lookahead = item1 [5] == item2 [5]

          return eq_base and eq_nprod and eq_at and eq_lookahead
        end,

        __name = 'ItemRule',

        __tostring = function (item)

          local base = item [1]
          local nprod = item [2]
          local at = item [3]
          local kernel = item [4]
          local lookahead = item [5]

          local meta = getmetatable (base).__tostring
          local tostr = function (e) return meta (e, true) end

          local look = not lookahead and '' or ', ' .. tostr (lookahead)
          local prods = List.map (linesof [base] [nprod], tostr)
          local prod = table.concat (List.insert (prods, at, '•'), ' ')
          local state = not kernel and '' or '*'

          return ('[%s -> %s%s]%s'):format (tostr (base), prod, look, state)
        end
      }

    --- @generic T: ItemLR0Rule | ItemLR1Rule
    --- @param self List<T>
    --- @param other T
    --- @return List<T> self
    --- @return boolean was_added
    ---
    function Item.add (self, other)

      if (Item.has (self, other)) then

        return self, false
      else

        return List.append (self, other), true
      end
    end

    --- @generic T: ItemLR0Rule | ItemLR1Rule
    --- @param self List<T>
    --- @param other List<T>
    --- @return boolean was_added
    ---
    function Item.equ (self, other)

      if (List.len (self) ~= List.len (other)) then

        return false
      else

        for _, item in ipairs (self) do

          if (not Item.has (other, item)) then return false end
        end
        return true
      end
    end

    --- @generic T: ItemLR0Rule | ItemLR1Rule
    --- @param self List<T>
    --- @param other T
    --- @return boolean
    ---
    function Item.has (self, other)

      for _, item in ipairs (self) do
        if (item == other) then return true end
      end
      return false
    end

    --- @generic T: ItemLR0Rule | ItemLR1Rule
    --- @param ... T
    --- @return List<T>
    ---
    function Item.new (...)

      return List (...)
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

    return Item
  end
return constructor
end
