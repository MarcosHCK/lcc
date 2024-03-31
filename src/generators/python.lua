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
local Action = require ('algorithms.action')
local Captures = require ('algorithms.captures')
local List = require ('pl.List')
local Map = require ('pl.Map')
local OrderedMap = require ('pl.OrderedMap')
local seq = require ('pl.seq')
local template = require ('pl.template')
local utils = require ('pl.utils')

--- @module 'generators'
--- @class PythonGenerator: Generator
local generator = {}

local yield, cowrap = coroutine.yield, coroutine.wrap

do

  local meta =
    {
      __index = function (_, k)

        return generator[k] or _G[k]
      end,
    }

  function generator.backrefs (symbols)

    local fn = function ()

      local i = 0

      for _, symbol in OrderedMap.iter (symbols) do

        --- @cast symbol TerminalSymbol
        i = i + 1

        if (symbol.terminal and symbol.id ~= nil) then

          yield (i, generator.escape (symbol.id))
        elseif (symbol.terminal) then

          for _, restriction in ipairs (symbol.restrictions) do

            yield (i, generator.escape (restriction))
          end
        end
      end
    end
  return cowrap (fn)
  end

  function generator.classes (symbols)

    local fn = function ()

      local i = 0

      for _, symbol in OrderedMap.iter (symbols) do

        --- @cast symbol Symbol
        i = i + 1

        if (symbol.id ~= nil and symbol.terminal) then

          yield (generator.escape (symbol.id), i)
        end
      end
    end
  return cowrap (fn)
  end

  function generator.escape (value)

    utils.assert_string (1, value)
    return string.gsub (value, '\'', '\\\'')
  end

  function generator.restrictions (symbols)

    local fn = function ()

      local i = 0

      for _, symbol in OrderedMap.iter (symbols) do

        i = i + 1

        if (symbol.id == nil) then

          for _, restriction in ipairs (symbol.restrictions) do

            yield (generator.escape (restriction), i)
          end
        end
      end
    end
  return cowrap (fn)
  end

  ---
  --- @param prolog TriggerFunc
  --- @param parser ParserTable
  --- @return string
  ---
  function generator.emit (prolog, parser)

    local name = 'generators/python.py'
    local file = assert (io.open (name, 'r'))
    local data = assert (file:read ('*a'))

    file:close ()

    local actions = parser.actions
    local captures = parser.captures
    local gotos = parser.gotos
    local items = parser.items
    local symbols = parser.symbols

    local backref = Map { }

    local global =
      {
        Action = Action,
        actions = actions,
        backref = backref,
        captures = captures,
        Captures = Captures,
        gotos = gotos,
        items = items,
        List = List,
        Map = Map,
        OrderedMap = OrderedMap,
        parser = parser,
        prolog = prolog,
        seq = seq,
        symbols = symbols,
        utils = utils,
      }

    local options =
      {
        chunk_name = '=' .. name,
        escape = '##',
        inline_escape = 'f',
        inline_brackets = '\"\"',
      }

    for i, _, symbol in seq.enum (OrderedMap.iter (parser.symbols)) do

      backref [symbol] = i
    end

    local ct = assert (template.compile (data, options))
    local rr = assert (ct:render (nil, setmetatable (global, meta)))
    return rr
  end
return generator
end
