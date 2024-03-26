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
local constructor

--- @class FollowSet: Set<TerminalSymbol>

do
  --- @param nthSymbol NthFunc
  --- @param linesof LinesOf
  --- @param firstsof FirstSet
  --- @param eof EofSymbol
  --- @return ParamInstance
  ---
  function constructor (nthSymbol, linesof, firstsof, initial, eof)

    --- @class Follow: ParamInstance
    local Follow = { }

    --- @param item ItemLR0
    --- @param symbol_ NonTerminalSymbol
    --- @return FollowSet
    ---
    function Follow.new (item, symbol_)

      local inner, register
      local got = { [initial] = Set { eof } }

      --- @generic T1
      --- @param follow T1
      --- @return T1 follow
      ---
      function register (symbol, follow)

        got [symbol] = follow
        return follow
      end

      --- @param symbol NonTerminalSymbol
      --- @return FollowSet
      ---
      inner = function (symbol, fixedat)

        if (got [symbol] ~= nil and got [symbol] == true) then

          return Set {}
        elseif (got [symbol] ~= nil) then

          return got [symbol]
        else

          local follow = Set {}

          --- @cast follow FollowSet
          got [symbol] = true

          for _, rule in ipairs (item) do

            local ats = List {}
            local base, nprod, at_ = utils.unpack (rule)

            if (fixedat) then

              if (nthSymbol (base, nprod, at_) == symbol) then

                List.append (ats, at_)
              end
            else

              local lines = assert (linesof [base])
              local line = assert (lines [nprod])

              for i, nth in ipairs (line) do

                if (nth == symbol) then

                  List.append (ats, i)
                end
              end
            end

            for _, at in ipairs (ats) do

              local next = nthSymbol (base, nprod, at + 1)

              if (not next or next == eof) then

                follow = follow + inner (base)
              elseif (next.terminal) then

                follow = follow + Set { next }
              else

                --- @cast next NonTerminalSymbol

                if (tablex.size (firstsof [next]) == 0) then

                  follow = follow + inner (next)
                else

                  follow = follow + firstsof [next]
                end
              end
            end
          end
          return register (symbol, follow)
        end
      end
      return inner (symbol_)
    end

    return Follow
  end
return constructor
end
