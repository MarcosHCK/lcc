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
local ClosuresOf = require ('algorithms.lr.closures')
local Grammar = require ('templates.grammar')
local LinesOf = require ('algorithms.lr.lines')
local List = require ('pl.List')
local utils = require ('pl.utils')
local tablex = require ('pl.tablex')

--- @module 'algorithms'
--- @class LRAlgorithm: Algorithm
local algorithm = {}

--- @class Tuple<T1, T2>: { [1]: T1, [2]: T2 }
--- @class Registry<T>: table<NonTerminalSymbol, boolean | T>
--- @alias NthFunc fun(s: NonTerminalSymbol, p: integer, n: integer)

--- @class ParamInstance: table<string, fun()>
--- @alias ParamConstructor fun(...: any): ParamInstance

do
  --- @param grammar Grammar
  --- @return ParserTable
  ---
  function algorithm.emit (grammar)

    grammar = Grammar._copy (grammar, true)

    local initials = Grammar._filter (grammar, function (_, e)

      --- @cast e NonTerminalSymbol
      return not e.terminal and e.initial == true
    end)

    local eof = assert (grammar.EOF)
    local nons = Grammar._filter (grammar, function (_, e) return not e.terminal end)
    local initial = assert (initials [tablex.keys (initials) [1]])

    --- @cast eof EofSymbol
    --- @cast nons NonTerminalSymbol[]
    --- @cast initial NonTerminalSymbol

    do

      local initial1 = initial
      local initial2 = Grammar._automate (grammar)

      initial1.initial = false
      initial2.initial = true
      initial = initial2

      List.append (nons, initial2)
      grammar:produce (initial2, initial1 + eof)
    end

    local linesof = LinesOf.new (nons)

    --- @param symbol NonTerminalSymbol
    --- @param nprod integer
    --- @param nth integer
    --- @return Symbol
    ---
    local function nthSymbol (symbol, nprod, nth)

      local lines = assert (linesof [symbol])
      local line = assert (lines [nprod])
      return line[nth]
    end

    local closuresof, firstsof = ClosuresOf.new (nthSymbol, nons)

    local Follow = require ('algorithms.lr.follow') (nthSymbol, linesof, firstsof, initial, eof)
    local Item = require ('algorithms.lr.item') (nthSymbol, closuresof, Follow)

    print (closuresof)
    print (firstsof)

    local itemlr0 = Item.lr0 ({ { initial, 1, 1, true } })
    local itemlr1 = Item.lr1 (itemlr0)

    for i, rule in ipairs (itemlr0) do

      --- @param e Ast
      local s = function (e) return getmetatable (eof).__tostring (e, true) end

      print (('rule0 (%d): %s %d %s'):format (i, ('%s -> %s'):format (s (rule [1]), s (rule [1].productions [rule [2]])), rule [3], rule [4] and 'kernel' or 'closure'))
    end
    for _, symbol in pairs (nons) do

      --- @param e Ast
      local s = function (e) return getmetatable (eof).__tostring (e, true) end

      print (('follow (%s): %s'):format (s (symbol), tostring (Follow.new (itemlr0, symbol))))
    end
    for i, rulek in ipairs (itemlr1) do

      local rule, T = utils.unpack (rulek)
      --- @param e Ast
      local s = function (e) return getmetatable (eof).__tostring (e, true) end

      print (('rulek (%d): %s; %d %s'):format (i, ('%s -> %s, %s'):format (s (rule [1]), s (rule [1].productions [rule [2]]), tostring (T)), rule [3], rule [4] and 'kernel' or 'closure'))
    end

    print (grammar)
    return {}
  end
  return algorithm
end
