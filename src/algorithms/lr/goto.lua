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

--- @module 'algorithms.lr.closure'
--- @module 'algorithms.lr.item'

--- @alias Goto table<GotoOn, Rule[]>
--- @class GotoOn: { [1]: Item, [2]: Symbol }

do
  ---
  --- @param nthSymbol NthFunc
  --- @param Closure Closure
  --- @param Item Item
  --- @param Rule Rule
  --- @return Closure
  ---
  function constructor (nthSymbol, Closure, Item, Rule)

    --- @param item_ Item
    --- @param x TerminalSymbol
    --- @return Item
    ---
    local function expand (item_, x)

      local item = utils.assert_arg (1, item_, 'table', Item.is, 'not an Item')
      local result = Item.new ()

      for _, rule in Item.iter (item) do

        local base, nprod, at, kk, t = utils.unpack (rule)
        local symbol = nthSymbol (base, nprod, at)

        if (symbol == x) then

          result = Item.add (result, Rule.new (base, nprod, at + 1, true, t))
        end
      end
      return Closure [result]
    end
    return setmetatable ({}, { __index = function (_, k) return expand (k[1], k[2]) end })
  end
return constructor
end
