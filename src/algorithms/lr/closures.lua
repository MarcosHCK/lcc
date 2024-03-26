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

--- @alias Closure List<Tuple<NonTerminalSymbol, integer>>
--- @alias FirstSet Set<TerminalSymbol>

--- @class ClosuresOf: table<NonTerminalSymbol, Closure>
local ClosuresOf = { }

do
  --- @param nthSymbol NthFunc
  --- @param symbol_ NonTerminalSymbol
  --- @param got Registry<Tuple<Closure, FirstSet>>
  --- @return Closure
  --- @return FirstSet
  ---
  local function forsymbol (nthSymbol, symbol_, got)

    local collect, inner, register

    --- @generic T1, T2
    --- @param closure T1
    --- @param first T2
    --- @return T1 closure
    --- @return T2 first
    ---
    collect = function (closure, first, ...)

      local r1, r2 = ...

      closure = List.extend (closure, r1)
      first = Set.union (first, r2)
      return closure, first
    end

    --- @generic T1, T2
    --- @param closure T1
    --- @param first T2
    --- @return T1 closure
    --- @return T2 first
    ---
    register = function (symbol, closure, first)

      got [symbol] = { closure, first }
      return closure, first
    end

    --- @param symbol NonTerminalSymbol
    --- @return Closure
    --- @return FirstSet
    ---
    inner = function (symbol)

      if (got [symbol] ~= nil and got [symbol] == true) then

        return List { }, Set { }
      elseif (got [symbol] ~= nil) then

        return utils.unpack (got [symbol])
      else

        local closure = List { }
        local first = Set { }
        local sym

        --- @cast closure Closure
        --- @cast first FirstSet
        got [symbol] = true

        for i in ipairs (symbol.productions or { }) do

          sym = nthSymbol (symbol, i, 1)

          --- @type Closure
          closure = not sym and closure or List.append (closure, { symbol, i })

          if (sym ~= nil and sym.terminal and not sym.epsilon) then

            first = first + Set { sym }
          elseif (sym ~= nil) then

            --- @cast sym NonTerminalSymbol
            closure, first = collect (closure, first, inner (sym))
          end
        end
      return register (symbol, closure, first)
      end
    end
    return inner (symbol_)
  end

  --- @param nthSymbol NthFunc
  --- @param nons table<string, NonTerminalSymbol>
  --- @return ClosuresOf
  --- @return FirstSet
  ---
  function ClosuresOf.new (nthSymbol, nons)

    local closuresof = { }
    local firstsof = { }
    local got = { }

    for _, symbol in pairs (nons) do

      local closure, first = forsymbol (nthSymbol, symbol, got)

      closuresof [symbol] = closure
      firstsof [symbol] = first
    end

    do
      local _, first = next (nons)

      --- @type fun(e: Ast): string
      local strfy = function (e) return getmetatable (first).__tostring (e, true) end

      local cc = function (s, c) return ('closure (%s): %s'):format (strfy (s), tostring (tablex.imap (function (e) return strfy (e [1].productions [e [2]]) end, c))) end
      local cf = function (s, f) return ('first (%s): %s'):format (strfy (s), tostring (tablex.imap (strfy, Set.values (f)))) end

      setmetatable (closuresof, { __tostring = function (t) return table.concat (tablex.map2 (cc, tablex.keys (t), tablex.values (t)), '\n') end })
      setmetatable (firstsof, { __tostring = function (t) return table.concat (tablex.map2 (cf, tablex.keys (t), tablex.values (t)), '\n') end })
    end
    return closuresof, firstsof
  end
return ClosuresOf
end
