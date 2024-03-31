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
local LinesOf = require ('algorithms.lr.lines')

--- @module 'algorithms'
--- @class LRAlgorithm: Algorithm
local algorithm = {}

--- @class Tuple<T1, T2>: { [1]: T1, [2]: T2 }
--- @class Registry<T>: table<NonTerminalSymbol, boolean | T>

--- @alias NthFunc fun(s: NonTerminalSymbol, p: integer, n: integer?): Symbol | Symbol[]
--- @alias SetConstructor<T> fun(...: any): T

do
  --- @param grammar Grammar
  --- @return ParserTable
  ---
  function algorithm.emit (grammar)

    grammar = Grammar._copy (grammar, true)

    local eof = assert (Grammar._get (grammar, Grammar.EOF))
    local epsilon = assert (Grammar._get (grammar, Grammar.EPSILON))
    local initial = assert (grammar:initial ())

    --- @cast eof EofSymbol
    --- @cast epsilon EpsilonSymbol

    do

      local initial1 = initial
      local initial2 = Grammar._automate (grammar)

      initial1.initial = false
      initial2.initial = true
      initial = initial2

      grammar:produce (initial2, initial1)
    end

    local nons = Grammar._filter (grammar, function (_, e) return not e.terminal end)
    local linesof = LinesOf.new (nons)

    --- @param symbol NonTerminalSymbol
    --- @param nprod integer
    --- @param nth? integer
    --- @return Symbol?
    ---
    local function nthSymbol (symbol, nprod, nth)

      local lines = assert (linesof [symbol])
      local line = assert (lines [nprod])
      return nth == nil and line or line [nth]
    end

    local symbols = Grammar._filter (grammar, function (_, e) return e ~= epsilon end)

    local Rule = require ('algorithms.lr.rule') (linesof)
    local Item = require ('algorithms.lr.item') (Rule)
    local Items = require ('algorithms.lr.items') (Item)
    local First = require ('algorithms.lr.first') (linesof, epsilon, symbols)
    local Closure = require ('algorithms.lr.closure') (nthSymbol, First, Item, Rule)
    local Goto = require ('algorithms.lr.goto') (nthSymbol, Closure, Item, Rule)
    local Table = require ('algorithms.lr.table') (nthSymbol, Goto, Item, Items, eof, initial, symbols)
    local s0 = Closure [ Item.new { Rule.new (initial, 1, 1, true, eof) } ]

    ---@diagnostic disable-next-line: return-type-mismatch
    return Table.new (s0)
  end
  return algorithm
end
