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
local tablex = require ('pl.tablex')
local utils = require ('pl.utils')

--- @type ParamConstructor
local constructor = {}

--- @class ItemLR0: ItemRule[]
--- @class ItemLR1: Tuple<ItemRule, TerminalSymbol>[]
--- @class ItemRule: { [1]: NonTerminalSymbol, [2]: integer, [3]: integer, [4]: boolean }

do

  --- @param nthSymbol NthFunc
  --- @param closuresof ClosuresOf
  --- @param Follow Follow
  --- @return Item
  ---
  function constructor (nthSymbol, closuresof, Follow)

    --- @class Item: ParamInstance
    local Item = {}

    --- @param seed ItemLR0
    --- @return ItemLR0
    ---
    function Item.lr0 (seed)

      local result = List {}

      for _, rule in ipairs (seed) do

        local symbol, nprod, at = utils.unpack (rule)
        local sym = nthSymbol (symbol, nprod, at)

        result = List.append (result, { symbol, nprod, at, true })

        if (sym ~= nil and not sym.terminal) then

          local closure
          closure = assert (closuresof [sym])
          result = List.extend (result, tablex.imap (function (e) return { e[1], e[2], 1, false } end, closure))
        end
      end
      return result
    end

    --- @param seed ItemLR0
    --- @return ItemLR1
    ---
    function Item.lr1 (seed)

      local itemk = List { }

      for _, rule in ipairs (seed) do

        local base = utils.unpack (rule)
        local follow = Follow.new (seed, base)

        for terminal in pairs (follow) do

          List.append (itemk, { rule, terminal })
        end
      end
      return itemk
    end

    return Item
  end
return constructor
end
