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
local Grammar = require ('grammar')
local List = require ('pl.List')
local Map = require ('pl.Map')

do
  --- @param out Grammar
  --- @param lookup table<Symbol, Symbol>
  --- @param epsilon EpsilonSymbol
  --- @param symbol NonTerminalSymbol
  --- @param production Operand
  --- @param inner? boolean
  --- @return List<Operand> tails
  ---
  local function breakdown (out, lookup, epsilon, symbol, production, inner)

    if (not inner) then

      local tails

      inner = true
      tails = breakdown (out, lookup, epsilon, symbol, production, inner)

      for _, tail in ipairs (tails) do

        out:produce (symbol, tail)
      end
      return tails
    elseif (production.type == 'symbol') then

      return List { lookup[production] }
    elseif (production.type == 'operator' and production.kind == '&') then

      --- @cast production BinaryOperator
      local tails1 = breakdown (out, lookup, epsilon, symbol, production.operand1, inner)
      local tails2 = breakdown (out, lookup, epsilon, symbol, production.operand2, inner)
      local tails = List { }

      local empty1 = List.len (tails1) == List.len (List.filter (tails1, function (e) return e.epsilon == true end))
      local empty2 = List.len (tails2) == List.len (List.filter (tails2, function (e) return e.epsilon == true end))
      local onempty = empty1 or empty2

      if (onempty) then

        List.extend (tails, empty2 and tails1 or tails2)

      else

        for _, tail1 in ipairs (tails1) do
        for _, tail2 in ipairs (tails2) do

          List.append (tails, tail1 + tail2)
        end end
      end
      return tails
    elseif (production.type == 'operator' and production.kind == '|') then

      --- @cast production BinaryOperator
      local tails = List {}
      List.extend (tails, breakdown (out, lookup, epsilon, symbol, production.operand1, inner))
      List.extend (tails, breakdown (out, lookup, epsilon, symbol, production.operand2, inner))
      return tails
    elseif (production.type == 'operator' and production.kind == '*') then

      --- @cast production UnaryOperator
      local tails
      local temp = Grammar._automate (out)

      tails = breakdown (out, lookup, epsilon, symbol, production.operand1, inner)
      tails = List.append (List.map (tails, function (e) return e + temp end), lookup[epsilon])
      List.foreach (tails, function (e) out:produce (temp, e) end)
      return List { temp }
    elseif (production.type == 'operator' and production.kind == '+') then

      --- @cast production UnaryOperator
      return breakdown (out, lookup, epsilon, symbol, production.operand1 + production.operand1 ^ 0, inner)
    elseif (production.type == 'operator' and production.kind == '?') then

      --- @cast production UnaryOperator
      return breakdown (out, lookup, epsilon, symbol, production.operand1 * epsilon, inner)
    else

      error (('unknown AST node %s'):format (production))
    end
  end

  ---
  --- Creates a cononical Grammar instance from in
  ---
  --- @param in_ Grammar
  --- @return Grammar
  ---
  local function canonicalize (in_)

    local out = Grammar._copy (in_)
    local symbols = Grammar._filter (in_, function () return true end)
    local epsilon = assert (Grammar._get (in_, Grammar.EPSILON))
    local lookup = Map { }

    --- @cast epsilon EpsilonSymbol

    for id, symbol in pairs (symbols) do

      lookup [symbol] = assert (Grammar._get (out, id))
    end

    for id, symbol in pairs (symbols) do

      if (not symbol.terminal) then

        local sym = Grammar._get (out, id)

        --- @cast symbol NonTerminalSymbol
        --- @cast sym NonTerminalSymbol

        for _, production in ipairs (symbol.productions or {}) do

          --- @cast production Operand
          breakdown (out, lookup, epsilon, sym, production)
        end
      end
    end
    return out
  end

  return canonicalize
end
