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

--- @type SetConstructor<First>
local constructor

--- @alias First table<Symbol | Symbol[], Set<TerminalSymbol>>

do
  ---
  --- @param linesof LinesOf
  --- @param epsilon EpsilonSymbol
  --- @param symbols Symbol[]
  --- @return First
  ---
  function constructor (linesof, epsilon, symbols)

    --- @type First
    local First = {}

    --- @type table<Symbol, Set<TerminalSymbol>>
    local firstof = {}

    for _, symbol in pairs (symbols) do

      if (symbol.terminal) then

        firstof [symbol] = Set { symbol }
      else

        firstof [symbol] = Set { }
      end
    end

    --- @param key Symbol | Symbol[]
    --- @return Set<TerminalSymbol>
    ---
    local function lookup (key)

      if (key [1]) then

        return firstof [key [1]]
      else

        return firstof [key [1]] or firstof [key]
      end
    end

    --- @param set1 Set<TerminalSymbol>
    --- @param set2 Set<TerminalSymbol>
    --- @param changed boolean
    --- @return Set<TerminalSymbol> updated_set1
    --- @return boolean updated_changed
    ---
    local function update (set1, set2, changed)

      if (Set.len (set2 - set1) == 0) then

        return set1, changed
      else

        return set1 + (set2 - Set { epsilon }), true
      end
    end

    repeat

      local changed = false

      for a, lines in pairs (linesof) do

        for _, line in ipairs (lines) do

          local b1 = line [1]
          local bk = line [List.len (line)]

          if (b1 ~= nil and b1 ~= epsilon) then

            local sa = firstof [a]
            local sb1 = firstof [b1]

            firstof [a], changed = update (sa, sb1, changed)
          end

          for i = 1, List.len (line) - 1 do

            local sa = firstof [a]
            local bi, bip = line [i], line [i + 1]
            local sbi, sbip = firstof [bi], firstof [bip]

            if (bi ~= epsilon and sbi [epsilon]) then

              firstof [a], changed = update (sa, sbip, changed)
            end
          end

          if (bk ~= nil and firstof [bk] [epsilon]) then

            local sa = firstof [a]

            if (not sa [epsilon]) then

              changed = true
              firstof [a] = sa + Set { epsilon }
            end
          end
        end
      end
    until (not changed)

    return setmetatable (First, { __index = function (_, k) return lookup (k) end })
  end
return constructor
end
