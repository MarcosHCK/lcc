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

--- @meta
--- @module 'algorithms.action'
--- @module 'algorithms.captures'

--- @class Algorithm
--- @field emit fun(grammar: Grammar): ParserTable

--- @class ParserTable
--- @field actions table<integer, table<TerminalSymbol, Action>>
--- @field captures Captures
--- @field gotos table<integer, table<NonTerminalSymbol, string>>
--- @field items ParserTableItems
--- @field symbols OrderedMap<string, Symbol>

--- @class ParserTableItem: List<ParserTableRule>

--- @class ParserTableItems: List<ParserTableItem>
--- @field iter fun (): (fun (): integer?, ParserTableItem?)
--- @field size fun (): integer

--- @class ParserTableRule: { [1]: NonTerminalSymbol, [2]: integer, [3]: integer, [4]: boolean, [5]: TerminalSymbol }
--- @field size fun (): integer
