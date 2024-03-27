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
local Set = require ('pl.Set')
local tablex = require ('pl.tablex')
local utils = require ('pl.utils')

--- @type ParamConstructor
local constructor = {}

--- @class ItemLR0: ItemLR0Rule[]
--- @class ItemLR1: ItemLR1Rule[]

--- @class ItemLR0Rule: { [1]: NonTerminalSymbol, [2]: integer, [3]: integer, [4]: boolean }
--- @class ItemLR1Rule: { [1]: NonTerminalSymbol, [2]: integer, [3]: integer, [4]: boolean, [5]: TerminalSymbol }

do

  --- @param nthSymbol NthFunc
  --- @param closuresof ClosuresOf
  --- @param Follow Follow
  --- @return Item
  ---
  function constructor (nthSymbol, firstsof, closuresof, Follow)

    --- @class Item: ParamInstance
    local Item = {}

    --- @param seed ItemLR0
    --- @return ItemLR0 item
    ---
    function Item.lr0 (seed)

      local result = List {}

      for _, rule in ipairs (seed) do

        local base, nprod, at = utils.unpack (rule)
        local sym = nthSymbol (base, nprod, at)

        result = List.append (result, Item.rulelr0 (base, nprod, at, true))

        if (sym ~= nil and not sym.terminal) then

          local closure
          closure = assert (closuresof [sym])
          result = List.extend (result, tablex.imap (function (e) return Item.rulelr0 (e[1], e[2], 1, false) end, closure))
        end
      end
      return result
    end

    --- @param item ItemLR0
    --- @return ItemLR1 item
    ---
    function Item.lr1 (item)

      local result = List { }

      for _, rule in ipairs (item) do

        local base, nprod, at, kernel = utils.unpack (rule)
        local follow = firstsof ()

        for terminal in pairs (follow) do

          result = List.append (result, Item.rulelr1 (base, nprod, at, kernel, terminal))
        end
      end
      return result
    end

    --- @param item ItemLR0
    --- @return ItemLR0[] seeds
    ---
    function Item.next (item)

      --- @type table<NonTerminalSymbol, List<ItemLR0Rule>>
      local groups = { }

      for _, rule in ipairs (item) do

        local base, nprod, at = utils.unpack (rule)
        local symbol = nthSymbol (base, nprod, at)

        if (symbol ~= nil) then

          local group = groups [symbol] or List { }

          groups [symbol] = List.append (group, tablex.copy (rule))
        end
      end

      for _, rules in pairs (groups) do
      for _, rule in ipairs (rules) do

        rule[3] = rule[3] + 1
      end end
      return tablex.values (groups)
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
      return { base, nprod, at, kernel == true }
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
      return { base, nprod, at, kernel == true, terminal }
    end

    --- @param initial NonTerminalSymbol
    --- @return ItemLR1[]
    ---
    function Item.spawn (initial)

      local items = List { }
      local seeds = List { { Item.rulelr0 (initial, 1, 1, true) } }

      repeat

        local nexts = List { }

        for _, seed in ipairs (seeds) do

          local itemlr0 = Item.lr0 (seed)
          local itemlr1 = Item.lr1 (itemlr0)

          items = List.append (items, itemlr1)
          nexts = List.extend (nexts, Item.next (itemlr0))
        end

        print (List.map (nexts, function (s)

          return List.map (s, function (e)

            return ('<%s, %i, %i, %s>'):format (e[1].id, e[2], e[3], e[4] and 'kernel' or 'closure', e[5])
          end)
        end))
        --seeds = nexts
        seeds = List {}
      until (List.len (seeds) == 0)
    return items
    end

    return Item
  end
return constructor
end
