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
local Action = require ('algorithms.lr.action')
local List = require ('pl.List')
local Map = require ('pl.Map')
local tablex = require ('pl.tablex')
local utils = require ('pl.utils')

--- @type SetConstructor<Closure>
local constructor

--- @alias Actions table<integer, table<TerminalSymbol, integer>>
--- @alias Gotos table<integer, table<NonTerminalSymbol, integer>>

do

  local function rpad (s, t)

    local l = string.len (s)
    local r = assert (t > l and t) - l
    return string.rep (' ', r) .. s
  end

  ---
  --- @param nthSymbol NthFunc
  --- @param Goto Goto
  --- @param Item Item
  --- @param eof EofSymbol
  --- @param initial NonTerminalSymbol
  --- @param symbols table<string, Symbol>
  --- @return Table
  ---
  function constructor (nthSymbol, Goto, Item, eof, initial, symbols)

    --- @class Table
    local Table = { }

    local function sorts (id1, id2)

      local t1 = symbols [id1].terminal
      local t2 = symbols [id2].terminal

      if (t1 == t2) then return id1 < id2 else return not t2 end
    end

    ---
    --- @param items List<ItemLR1>
    --- @param actions Actions
    --- @param gotos Gotos
    --- @return string
    ---
    function Table.write (items, actions, gotos, colspan, vhspan)

      local lines = { }

      colspan = not colspan and 13 or utils.assert_arg (4, colspan, 'number')
      vhspan = not vhspan and 3 or utils.assert_arg (5, vhspan, 'number')

      local function vh (state) return '|' .. rpad (state, vhspan) .. '  |' end
      local function col (content) return rpad (content, colspan) .. ' |' end

      do

        local line = vh ('')

        for id in tablex.sort (symbols, sorts) do line = line .. col (id) end

        lines = List.append (lines, '+' .. string.rep ('-', line:len () - 2) .. '+')
        lines = List.append (lines, line)
        lines = List.append (lines, '+' .. string.rep ('-', line:len () - 2) .. '+')
      end

      for state = 1, List.len (items) do

        local bits = { vh (tostring (state)) }

        for _, symbol in tablex.sort (symbols, sorts) do

          if (not symbol.terminal) then

            bits = List.append (bits, col (tostring (gotos [state] [symbol] or 0)))
          else

            bits = List.append (bits, col (tostring (actions [state] [symbol] or '')))
          end
        end

        bits = List.append (bits)
        lines = List.append (lines, table.concat (bits))
      end
    return table.concat (lines, '\n')
    end

    ---
    --- @param s0 ItemLR1
    --- @return ItemLR1[] items
    --- @return Actions action_table
    --- @return Gotos goto_table
    ---
    function Table.new (s0)

      local gotor = Map { }
      local items = List { s0 }
      local kur = 2

      --
      -- Create the table items
      --

      do
        local i, item = nil, nil

        while true do

          i, item = next (items, i)

          if (item ~= nil) then

            for _, symbol in pairs (symbols) do

              local goto_ = Goto [{ item, symbol }]
              local k

              if (List.len (goto_) > 0) then

                k = tablex.find_if (items, Item.equ, goto_)
                gotor [i] = gotor [i] or Map { }

                if (k ~= nil) then

                  gotor [i] [symbol] = k
                else

                  gotor [i] [symbol] = kur
                  i = nil
                  items = List.append (items, goto_)
                  kur = kur + 1
                break end
              end
            end
          else break end
        end

        for j = 1, List.len (items) do gotor [j] = gotor [j] or Map { } end
      end

      local actions = Map { }
      local gotos = Map { }

      --
      -- Fill ACTION and GOTO tables
      --

      for i, item in ipairs (items) do

        actions [i] = Map { }
        gotos [i] = Map { }

        for _, symbol in pairs (symbols) do

          if (not symbol.terminal) then

            if (gotor [i] [symbol] ~= nil) then

              gotos [i] [symbol] = gotor [i] [symbol]
            end
          end
        end

        for j, rule in ipairs (item) do

          local base, nprod, at, _, t = utils.unpack (rule)
          local symbol = nthSymbol (base, nprod, at)

          if (symbol ~= nil and symbol.terminal and gotor [i] [symbol] ~= nil) then

            actions [i] [symbol] = Action.serialize ('shift', gotor [i] [symbol])
          elseif (not symbol and base == initial and t == eof) then

            actions [i] [t] = Action.serialize ('accept')
          elseif (not symbol) then

            actions [i] [t] = Action.serialize ('reduce', j)
          end
        end
      end
      return items, actions, gotos
    end
    return Table
  end
return constructor
end
