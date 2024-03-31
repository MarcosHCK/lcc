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
local utils = require ('pl.utils')

--- @type SetConstructor<Rule>
local constructor

local format = string.format

do
  ---
  --- @param linesof LinesOf
  ---
  function constructor (linesof)

    --- @class Rule
    --- @field public [1] NonTerminalSymbol
    --- @field public [2] integer
    --- @field public [3] integer
    --- @field public [4] boolean
    --- @field public [5] TerminalSymbol
    --- @field public hash string
    local Rule = {}

    local rule_mt =
      {
        __eq = function (self, other)

          local eq_base = self [1] == other [1]
          local eq_nprod = self [2] == other [2]
          local eq_at = self [3] == other [3]
          local eq_lookahead = self [5] == other [5]

          return eq_base and eq_nprod and eq_at and eq_lookahead
        end,

        __hash = function (self) return self.hash end,
        __index = function (_, k) return Rule[k] end,

        __name = 'ItemRule',

        __tostring = function (self)

          local kernel = self [4]
          local state = not kernel and '' or '*'

          return format ('%s%s', self.hash, state)
        end
      }

    --- @param base NonTerminalSymbol
    --- @param nprod integer
    --- @param at integer
    --- @param lookahead TerminalSymbol
    --- @return string
    ---
    local function serialize (base, nprod, at, lookahead)

      local meta = getmetatable (base).__tostring
      local tostr = function (e) return meta (e, true) end

      local look = not lookahead and '' or ', ' .. tostr (lookahead)
      local prods = List.map (linesof [base] [nprod], tostr)
      local prod = table.concat (List.insert (prods, at, '•'), ' ')

      return format ('[%s -> %s%s]', tostr (base), prod, look)
    end

    --- @type fun(arg: any): boolean
    function Rule.is (arg) return utils.is_type (arg, rule_mt) end

    ---
    --- @param base NonTerminalSymbol
    --- @param nprod integer
    --- @param at integer
    --- @param kernel boolean
    --- @param lookahead TerminalSymbol
    --- @return Rule
    ---
    function Rule.new (base, nprod, at, kernel, lookahead)

      utils.assert_arg (1, base, 'table')
      utils.assert_arg (5, lookahead, 'table')
      utils.assert_arg (2, nprod, 'number', function (n) return n == math.floor (n) end, 'not an integer')
      utils.assert_arg (3, at, 'number', function (n) return n == math.floor (n) end, 'not an integer')

      local hash = serialize (base, nprod, at, lookahead)
      local kernel = kernel == nil and false or utils.assert_arg (4, kernel, 'boolean')

      return setmetatable ({ base, nprod, at, kernel == true, lookahead, hash = hash }, rule_mt)
    end

    ---
    --- @return integer
    ---
    function Rule:size ()

      utils.assert_arg (0, self, 'table', Rule.is, 'not a Rule instance')
      return List.len (linesof [self[1]] [self[2]])
    end

    ---
    --- @return NonTerminalSymbol
    --- @return integer
    --- @return integer
    --- @return boolean
    --- @return TerminalSymbol
    ---
    function Rule:unpack ()

      utils.assert_arg (0, self, 'table', Rule.is, 'not a Rule instance')
      return self[1], self[2], self[3], self[4], self[5]
    end

    return Rule
  end
return constructor
end
