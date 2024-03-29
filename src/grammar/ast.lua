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

--- @class Ast
--- @field public type AstType

--- @alias Associativity 'left' | 'right'
--- @alias AstType 'operator' | 'symbol'
--- @alias Operand Operator | Symbol
--- @alias OperatorKind '&' | '|' | '*' | '+' | '?' | '$'
--- @alias TriggerFunc fun (...: any): string?

--- @class Operator: Ast
--- @field public kind OperatorKind

--- @class Symbol: Ast
--- @field public associativity? Associativity
--- @field public id? string
--- @field public precedence? integer
--- @field public terminal boolean

--- @class BinaryOperator: Operator
--- @field public operand1 Operand
--- @field public operand2 Operand

--- @class TriggerOperator: Operator
--- @field public callback TriggerFunc
--- @field public operand1 Operand

--- @class UnaryOperator: Operator
--- @field public operand1 Operand

--- @class EofSymbol: TerminalSymbol
--- @field public eof boolean

--- @class EpsilonSymbol: TerminalSymbol
--- @field public epsilon boolean

--- @class NonTerminalSymbol: Symbol
--- @field public initial? boolean
--- @field public productions? Operand[]

--- @class TerminalSymbol: Symbol
--- @field public restrictions? Set<string>
