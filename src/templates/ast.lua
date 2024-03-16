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
local Set = require ('pl.Set')

--- @class Ast
--- @field type AstType
local Ast = { }

--- @alias AstType
--- | 'operator'
--- | 'symbol'
--- | 'trigger'

--- @alias OperatorType
--- | '&' # BinaryOperator (BNF's (expr expr) concatenation)
--- | '|' # BinaryOperator (BNF's (expr | expr) or operator)
--- | '*' # UnaryOperator (EBNF's ( expr* ) zero-or-more operator)
--- | '+' # UnaryOperator (EBNF's ( expr+ ) one-or-more operator)
--- | '?' # UnaryOperator (EBNF's ( expr? ) optional operator)

--- @class Operator: Ast
--- @field op OperatorType

--- @class Symbol: Ast
--- @field name string
--- @field terminal boolean

--- @class Trigger: Ast
--- @field callback TriggerFunc
--- @field over Operand

--- @alias TriggerFunc fun(...)

--- @alias Operand Operator | Symbol

--- @class BinaryOperator: Operator
--- @field operand1 Operand
--- @field operand2 Operand

--- @class UnaryOperator: Operator
--- @field operand Operand

--- @class NonTerminalSymbol: Symbol
--- @field productions Operand[]

--- @class TerminalSymbol: Symbol
--- @field constraints? string[]

do

  local ast_mt

  ---
  --- Checks if arg is an AST instance
  ---
  --- @param arg any
  --- @return boolean is
  ---
  function Ast.isAst (arg) return type (arg) == 'table' and utils.is_type (arg, ast_mt) end

  ---
  --- Checks if arg is an AST instance of a given type
  ---
  --- @param arg any
  --- @param type AstType
  --- @return boolean is
  ---
  function Ast.isAstOfType (arg, type) return Ast.isAst (arg) and arg.type == type end

  ---
  --- Checks if arg is an instance of NonTerminalSymbol
  ---
  --- @param arg any
  --- @return boolean is
  ---
  function Ast.isNonTerminal (arg) return Ast.isSymbol (arg, false) end

  ---
  --- Check if arg is an instance of Operator
  ---
  --- @param arg any
  --- @param type? OperatorType
  --- @return boolean is
  ---
  function Ast.isOperator (arg, type) return Ast.isAstOfType (arg, 'operator') and (type == nil or arg.op == type) end

  ---
  --- Checks if arg is an instance of Symbol
  ---
  --- @param arg any
  --- @param terminal? boolean
  --- @return boolean is
  ---
  function Ast.isSymbol (arg, terminal) return Ast.isAstOfType (arg, 'symbol') and (terminal == nil or arg.terminal == terminal) end

  ---
  --- Checks if arg is an instance of TerminalSymbol
  ---
  --- @param arg any
  --- @return boolean is
  ---
  function Ast.isTerminal (arg) return Ast.isSymbol (arg, true) end

  ---
  --- Checks if arg is an instance of Trigger
  ---
  function Ast.isTrigger (arg) return Ast.isAstOfType (arg, 'trigger') end

  ---
  --- Creates a new symbol
  ---
  --- @param nameOrLeft string | Ast
  --- @param terminalOrRight? boolean | TriggerFunc | Ast
  --- @param operationType? OperatorType
  --- @return Ast symbol
  ---
  function Ast.new (nameOrLeft, terminalOrRight, operationType)

    local symbol = { }

    if (not Ast.isAst (nameOrLeft)) then

      --- @cast symbol Symbol
      symbol.name = utils.assert_string (1, nameOrLeft)
      symbol.terminal = utils.assert_arg (2, terminalOrRight, 'boolean')
      symbol.type = 'symbol'

    elseif (not terminalOrRight) then

      --- @cast symbol UnaryOperator
      symbol.op = utils.assert_arg (2, operationType, 'string')
      symbol.operand = utils.assert_arg (1, nameOrLeft, 'table')
      symbol.type = 'operator'
    elseif (not Ast.isAst (terminalOrRight)) then

      --- @cast symbol Trigger

      symbol.callback = utils.assert_arg (2, terminalOrRight, 'function')
      symbol.over = utils.assert_arg (1, nameOrLeft, 'table')
      symbol.type= 'trigger'
    else

      --- @cast symbol BinaryOperator
      symbol.op = utils.assert_arg (2, operationType, 'string')
      symbol.operand1 = utils.assert_arg (1, nameOrLeft, 'table')
      symbol.operand2 = utils.assert_arg (3, terminalOrRight, 'table', Ast.isAst, 'not a symbol')
      symbol.type = 'operator'
    end
    return setmetatable (symbol, ast_mt)
  end

  ---
  --- Creates a new non-terminal
  ---
  --- @param name string
  --- @return NonTerminalSymbol
  ---
  function Ast.nonTerminal (name)

    --- @type NonTerminalSymbol
    return Ast.new (name, false)
  end

  ---
  --- Adds a production to a non-terminal symbol
  ---
  --- @param symbol NonTerminalSymbol
  --- @param produces Operator | Symbol
  ---
  function Ast.produces (symbol, produces)

    local check = function (a) return Ast.isOperator (a) or Ast.isSymbol (a) end

    utils.assert_arg (1, symbol, 'table', Ast.isNonTerminal, 'not a non-terminal symbol')
    utils.assert_arg (2, produces, 'table', check, 'not a symbol')
    symbol.productions = List.append (symbol.productions or {}, produces)
  end

  ---
  --- Constraints the semantic value of the corresponding token
  ---
  --- @param symbol TerminalSymbol
  --- @param ... string
  ---
  function Ast.restrict (symbol, ...)

    utils.assert_arg (1, symbol, 'table', Ast.isTerminal, 'not a terminal symbol')
    for i, arg in ipairs ({ ... }) do utils.assert_string (i + 1, arg) end

    symbol.constraints = select ('#', ...) > 0 and utils.pack (...) or nil
  end

  ---
  --- Creates a new terminal
  ---
  --- @param name string
  --- @return TerminalSymbol
  ---
  function Ast.terminal (name)

    --- @type TerminalSymbol
    return Ast.new (name, true)
  end

  --- @param s Operand
  --- @return table<TerminalSymbol, boolean>
  local function collectNons (s)

    if (Ast.isNonTerminal (s)) then

      return Set { s }
    elseif (Ast.isOperator (s)) then

      --- @cast s UnaryOperator

      if (s.operand ~= nil) then

        return collectNons (s.operand)
      else

        --- @cast s BinaryOperator
        local op1 = collectNons (s.operand1)
        local op2 = collectNons (s.operand2)
        return Set.union (op1, op2)
      end
    end
    return Set { }
  end

  local function extractNons (s)

    local nexts = List { s }
    local nons = Set { s }

    while (List.len (nexts) > 0) do

      --- @type NonTerminalSymbol
      local next = List.pop (nexts)

      for _, p in ipairs (next.productions) do

        local news = Set.union (nons, collectNons (p))
        local adds = news - nons

        nexts = List.extend (nexts, Set.values (adds))
        nons = news
      end
    end
    return nons
  end

  ast_mt =
    {
      __add = function (s1, s2) return Ast.new (s1, s2, '&') end,

      ---
      --- @param s Ast
      --- @param c TriggerFunc
      ---
      __div = function (s, c)

        local check = function (e) return Ast.isSymbol (e) or Ast.isOperator (e) end

        utils.assert_arg (2, c, 'function')
        utils.assert_arg (1, s, 'table', check, 'wrong operation over a non-operand AST')
        --- @cast s Symbol | Operator
        return Ast.new (s, c)
      end,

      __index = function (_, k) return Ast[k] end,
      __mul = function (s1, s2) return Ast.new (s1, s2, '|') end,
      __name = 'Ast',

      __pow = function (s1, factor)

        utils.assert_arg (2, factor, 'number', nil, nil, 3)

        if (factor == 1) then return Ast.new (s1, nil, '+')
        elseif (factor == 0) then return Ast.new (s1, nil, '*')
        elseif (factor == -1) then return Ast.new (s1, nil, '?')
        else error (('invalid unary factor \'%d\''):format (factor))
        end
      end,

      ---
      --- @type fun(s: Ast, inline?: boolean): string
      ---
      __tostring = function (s, inline)

        if (Ast.isOperator (s)) then

          --- @cast s UnaryOperator
          if (s.operand ~= nil) then

            local op1 = ast_mt.__tostring (s.operand, true)
            return ('(%s)%s'):format (op1, s.op)
          else

            --- @cast s BinaryOperator
            local op1 = ast_mt.__tostring (s.operand1, true)
            local op2 = ast_mt.__tostring (s.operand2, true)
            return ('(%s %s %s)'):format (op1, s.op, op2)
          end
        elseif (Ast.isTerminal (s)) then

          --- @cast s TerminalSymbol
          if (not s.constraints) then

            return s.name
          else

            return ('%s:{%s}'):format (s.name, List.concat (s.constraints, ', '))
          end
        elseif (Ast.isTrigger (s)) then

          --- @cast s Trigger
          return ('$(%s)'):format (tostring (s.over))
        elseif (Ast.isNonTerminal (s)) then

          --- @cast s NonTerminalSymbol
          if (inline == true) then

            return s.name
          else

            local lines = List { }
            local nons = List (Set.values (extractNons (s)))

            for _, non in ipairs (nons) do

              for _, production in ipairs (non.productions or {}) do

                --- @cast production Operand
                local op1 = ast_mt.__tostring (production, true)

                lines = List.append (lines, ('%s -> %s'):format (non.name, op1))
              end
            end
            return List.concat (lines, '\n')
          end
        else

          error ('unknown AST node')
        end
      end,
    }

return Ast
end
