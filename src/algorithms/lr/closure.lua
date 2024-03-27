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

--- @type SetConstructor<Closure>
local constructor

--- @module 'algorithms.lr.first'
--- @module 'algorithms.lr.item'

--- @alias Closure table<ItemLR1, ItemLR1>

do
  ---
  --- @param nthSymbol NthFunc
  --- @param First First
  --- @param Item Item
  --- @return Closure
  ---
  function constructor (nthSymbol, First, Item)

    --- @param item ItemLR1
    --- @param rule ItemLR1Rule
    --- @param changed boolean
    --- @return ItemLR1 item
    --- @return boolean updated
    ---
    local function update (item, rule, changed)

      local result, added = Item.add (item, rule)
      return result, added or changed
    end

    --- @param item ItemLR1
    --- @return ItemLR1 closed_item
    ---
    local function expand (item)

      local result = List (item)

      repeat

        local changed = false

        for _, rule in ipairs (result) do

          local base, nprod, at, _, t = utils.unpack (rule)
          local symbol = nthSymbol (base, nprod, at)

          if (symbol ~= nil and not symbol.terminal) then

            --- @cast symbol NonTerminalSymbol
            --- @type Symbol[]

            local sigma = List.slice (nthSymbol (base, nprod), at + 1)

            for i in ipairs (symbol.productions or { }) do

              local follow = List.append (List.clone (sigma), t)
              local first = First [follow]

              for terminal in pairs (first) do

                local newr = Item.rulelr1 (symbol, i, 1, false, terminal)
                result, changed = update (result, newr, changed)
              end
            end
          end
        end
      until (not changed)
    return result
    end

    return setmetatable ({}, { __index = function (_, k) return expand (k) end })
  end
return constructor
end
