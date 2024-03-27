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
    local firstof = { }

    --- @param key Symbol | Symbol[]
    --- @return Set<TerminalSymbol>
    ---
    local function lookup (key)

      if (key.type) then

        return firstof [key]
      else

        for _, symbol in ipairs (key) do

          if (symbol ~= epsilon) then

            return firstof [symbol]
          end
        end
      return Set { }
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

        return set1 + set2, true
      end
    end

    local stack = List { }

    for _, symbol in pairs (symbols) do

      if (symbol.terminal) then

        firstof [symbol] = Set { symbol }
      else

        firstof [symbol] = Set { }
        stack = List.append (stack, symbol)
      end
    end

    repeat

      local changed = false
      local X = List.pop (stack)

      --- @cast X NonTerminalSymbol

      for j in pairs (X.productions or { }) do

        local P = linesof [X] [j] [1]
        local b = 2

        repeat

          local another = false

          if (P == epsilon) then

            firstof [X], changed = update (firstof [X], Set { epsilon })
          elseif (P ~= nil and P.terminal) then

            firstof [X], changed = update (firstof [X], firstof [P], changed)
          elseif (P ~= nil and P ~= X) then

            firstof [X], changed = update (firstof [X], firstof [P] - Set { epsilon }, changed)
          elseif (P ~= nil and firstof [P] [epsilon]) then

            local B = linesof [X] [j] [b]

            if (B ~= nil and not B.terminal) then

              P = B
              b = b + 1
              another = true
            end
          end
        until (not another)
      end

      if (changed or Set.len (firstof [X]) == 0) then stack = List.insert (stack, 1, X) end
    until (List.len (stack) == 0)

    return setmetatable (First, { __index = function (_, k) return lookup (k) end })
  end
return constructor
end
