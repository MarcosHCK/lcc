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
local Map = require ('pl.Map')
local OrderedMap = require ('pl.OrderedMap')
local yield, cowrap = coroutine.yield, coroutine.wrap

--- @alias Line Symbol[]
--- @alias LinesOf table<NonTerminalSymbol, Line[]>
local LinesOf = { }

do
  --- @param from Operand
  --- @return fun(): Symbol?
  ---
  local function itersymbols (from)

    local fn

    --- @param operand Operand
    ---
    fn = function (operand)

      if (operand.type == 'symbol') then yield (operand)
      elseif (operand.type == 'operator' and operand.kind == '&') then

        --- @cast operand BinaryOperator

        fn (operand.operand1)
        fn (operand.operand2)
      elseif (operand.type == 'operator' and operand.kind == '$') then

        --- @cast operand TriggerOperator

        fn (operand.operand1)
      else

        error ('unknown AST node ' .. tostring (operand))
      end
    end
  return cowrap (function () fn (from) end)
  end

  --- @param from NonTerminalSymbol
  --- @return List<Symbol[]>
  local function linearize (from)

    local lines = List { }

    for i, production in ipairs (from.productions or { }) do

      local j = 0
      lines[i] = { }

      for symbol in itersymbols (production) do

        j = j + 1
        lines[i][j] = symbol
      end
    end
    return lines
  end

  --- @param nons OrderedMap<string, NonTerminalSymbol>
  --- @return LinesOf
  ---
  function LinesOf.new (nons)

    local linesof = Map { }

    for _, symbol in OrderedMap.iter (nons) do

      linesof [symbol] = linearize (symbol)
    end
    return linesof
  end

return LinesOf
end
