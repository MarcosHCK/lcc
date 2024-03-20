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
local func = require ('pl.func')
local pretty = require ('pl.pretty')
local utils = require ('pl.utils')

--- @class List<T>: { [integer]: T }
--- @field append fun(self: List, item: any)
--- @field extend fun(self: List, other: List)
--- @field len fun(self: List): integer
--- @field concat fun(self: List, sep?: string): string
--- @field pop fun(self: List): any
local List = require ('pl.List')

--- @class Set<T>: { [T]: boolean }
--- @field len fun(self: Set): integer
--- @field union fun(a: Set, b: Set): Set
--- @field values fun(self: Set): List
local Set = require ('pl.Set')

--- @class Grammar
--- @field public associative fun(self: Grammar, symbol: string | Symbol, assoc: Associativity): Symbol
--- @field public literal fun(self: Grammar, value: string): Symbol
--- @field public nonterminal fun(self: Grammar, class: string): Symbol
--- @field public precedence fun(self: Grammar, symbol: string | Symbol, value: integer): Symbol
--- @field public produce fun(self: Grammar, symbol: string | Symbol, operand: Operand): Symbol, Operand
--- @field public restrict fun(self: Grammar, symbol: string | Symbol, values: string []): Symbol
--- @field public symbol fun(self: Grammar, class: string): Symbol
--- @field private symbols table<string>
--- @field public token fun(self: Grammar, id: string): Symbol
local Grammar = { }

--- @alias Associativity 'left' | 'right'
--- @alias AstType 'operator' | 'symbol'
--- @alias Operand Operator | Symbol
--- @alias OperatorKind '&' | '|' | '*' | '+' | '?' | '$'
--- @alias TriggerFunc fun()

--- @class Ast
--- @field public type AstType
local Ast = { }

--- @class Operator: Ast
--- @field public kind OperatorKind

--- @class Symbol: Ast
--- @field public associativity? Associativity
--- @field public id? string
--- @field public precedence? integer
--- @field public terminal boolean
--- @field public trigger? TriggerFunc

--- @class BinaryOperator: Operator
--- @field public operand1 Operand
--- @field public operand2 Operand

--- @class TriggerOperator: Operator
--- @field public callback TriggerFunc
--- @field public operand1 Operand

--- @class UnaryOperator: Operator
--- @field public operand1 Operand

--- @class NonTerminalSymbol: Symbol
--- @field public productions? Operand[]

--- @class TerminalSymbol: Symbol
--- @field public restrictions? Set<string>

do

  local grammar_mt = { __name = 'Grammar' }

  --- @param self Grammar
  --- @param name string
  --- @param symbol Symbol
  --- @return Symbol
  ---
  function Grammar._add (self, name, symbol)

    assert (not self.symbols[name], ('redefining symbol \'%s\''):format (name))

    self.symbols[name] = symbol
    return symbol
  end

  --- @param self Grammar
  --- @param class string
  --- @return Symbol
  ---
  function Grammar._get (self, class)

    return self.symbols[utils.assert_string (1, class)]
  end

  --- @param self Grammar
  --- @param value string | Symbol
  --- @return Symbol
  ---
  function Grammar._node (self, value)

    if (type (value) ~= 'string') then

      return (utils.assert_arg (1, value, 'table', Ast.isSymbol, 'not a symbol'))
    else

      local id = ('\'%s\''):format (value)
      local sym = self.symbols[id]

      return sym or self:restrict (Grammar._add (self, id, Ast.new ('symbol', nil, true)), { value })
    end
  end

  --- @param o Operand
  --- @return Set<TerminalSymbol>
  local function collectNons (o)

    if (Ast.isNonTerminal (o)) then return Set { o }
    elseif (Ast.isOperator (o)) then

      --- @cast o BinaryOperator
      return collectNons (o.operand1) + (collectNons (o.operand2 or {}))
    end
    return Set { }
  end

  --- @param o Operand
  --- @return Set<TerminalSymbol>
  local function extractNons (o)

    local nexts = List { o }
    local nons = Set { o }

    while (List.len (nexts) > 0) do

      for _, p in ipairs (List.pop (nexts).productions) do

        local news = Set.union (nons, collectNons (p))

        nexts = List.extend (nexts, Set.values (news - nons))
        nons = news
      end
    end
    return nons
  end

  ---
  --- Creates a Symbols instance
  ---
  --- @return Grammar
  ---
  function Grammar.new ()

    local ast_mt
    local grammar

    ---
    --- @param s Ast
    --- @param t AstType
    --- @param ... any
    ---
    function Ast.is (s, t, ...)

      if (not utils.is_type (s, ast_mt)) then

        return false
      elseif (t ~= nil and s.type ~= t) then

        return false
      elseif (t ~= nil) then

        if (t == 'operator') then

          --- @cast s Operator
          --- @type OperatorKind
          local kind = ...
          return kind == nil or s.kind == kind
        elseif (t == 'symbol') then

          --- @cast s Symbol
          --- @type boolean
          local terminal = ...
          return terminal == nil or s.terminal == terminal
        else

          error (('unknown AST node: %s'):format (pretty.write (t)))
        end
      end
      return true
    end

    ---
    --- @param t AstType
    --- @param ... any
    ---
    function Ast.new (t, ...)

      local node = { }

      if (t == 'operator' and type ((...)) == 'function') then

        local a1, a2 = ...
        local fn = utils.assert_arg (2, a1, 'function')
        local op1 = utils.assert_arg (3, a2, 'table', Ast.isOperand, 'not an operand')
        node = { callback = fn, kind = '$', operand1 = op1 }
      elseif (t == 'operator') then

        local a1, a2, a3 = ...
        local kind = utils.assert_string (2, a1)
        local op1 = utils.assert_arg (3, a2, 'table', Ast.isOperand, 'not an operand')
        local op2 = a3 and utils.assert_arg (4, a3, 'table', Ast.isOperand, 'not an operand')
        node = { kind = kind, operand1 = op1, operand2 = op2 or nil }
      elseif (t == 'symbol') then

        local a1, a2 = ...
        local id = a1 and utils.assert_arg (2, a1, 'string')
        local terminal = utils.assert_arg (3, a2, 'boolean')
        node = { id = id or nil, productions = not terminal and List { }, restrictions = terminal and Set { }, terminal = terminal }
      end

      node.type = utils.assert_arg (1, t, 'string')

      return setmetatable (node, ast_mt)
    end

    --- @type fun(s: any, type?: AstType, ...: any): boolean
    Ast.isAst = function (s, type) return Ast.is (s, type) end
    --- @type fun(s: any): boolean
    Ast.isNonTerminal = function (s) return Ast.is (s, 'symbol', false) end
    --- @type fun(s: any): boolean
    Ast.isOperand = function (s) return Ast.isOperator (s) or Ast.isSymbol (s) end
    --- @type fun(s: any, kind?: OperatorKind): boolean
    Ast.isOperator = function (s, kind) return Ast.is (s, 'operator', kind) end
    --- @type fun(s: any, terminal?: boolean): boolean
    Ast.isSymbol = function (s) return Ast.is (s, 'symbol') end
    --- @type fun(s: any): boolean
    Ast.isTerminal = function (s) return Ast.is (s, 'symbol', true) end

    --- @type fun(kind: OperatorKind, op1: Operand, op2?: Operand): Operator
    Ast.newOperator = func.bind1 (Ast.new, 'operator')
    --- @type fun(id?: string, terminal: boolean): Symbol
    Ast.newSymbol = func.bind1 (Ast.new, 'symbol')
    --- @type fun(callback: TriggerFunc, op1: Operand): Operator
    Ast.newTrigger = func.bind1 (Ast.new, 'operator')

    --- @type fun(o: string | Operand): Operand
    ---
    Ast.operand = function (o)

      if (type (o) == 'string') then

        return Grammar._node (grammar, o)
      else

        return utils.assert_arg (1, o, 'table', Ast.isOperand, 'not an operand')
      end
    end

    ast_mt =
      {
        --- @type fun(a: Operand, b: string | Operand): Operator
        __add = function (a, b) return Ast.newOperator ('&', Ast.operand (a), Ast.operand (b)) end,
        --- @type fun(a: Operand, b: TriggerFunc): Operator
        __div = function (t, c) return Ast.newTrigger (c, Ast.operand (t)) end,
        --- @type fun(a: Operand, b: string | Operand): Operator
        __mul = function (a, b) return Ast.newOperator ('|', Ast.operand (a), Ast.operand (b)) end,

        __name = 'AST',

        --- @type fun(t: Operand, n: integer): Operator
        ---
        __pow = function (t, n)

          local at = utils.assert_arg (1, n, 'number', function (e) return e == math.floor (e) end, 'not an integer')

          if (at == 0) then return Ast.newOperator ('*', Ast.operand (t))
          elseif (at == 1) then return Ast.newOperator ('+', Ast.operand (t))
          elseif (at == -1) then return Ast.newOperator ('?', Ast.operand (t))
          else error (('unknown operation %s^%s'):format (tostring (t), tostring (n)))
          end
        end,

        --- @type fun(t: Ast, inline: boolean): string
        ---
        __tostring = function (t, inline)

          if (Ast.isOperator (t)) then

            --- @cast t BinaryOperator
            if (not t.operand2) then

              local op1 = ast_mt.__tostring (t.operand1, true)
              return ('(%s)%s'):format (op1, t.kind)
            else

              --- @cast t BinaryOperator
              local op1 = ast_mt.__tostring (t.operand1, true)
              local op2 = ast_mt.__tostring (t.operand2, true)
              return ('(%s %s %s)'):format (op1, t.kind, op2)
            end
          elseif (Ast.isTerminal (t)) then

            --- @cast t TerminalSymbol

            if (not t.id) then

              assert (List.len (t.restrictions) == 1, 'literal symbol must have exactly one restriction')
              return ('\'%s\''):format (t.restrictions[1])
            elseif (Set.len (t.restrictions) == 0) then
              return t.id
            else
              return ('%s:{%s}'):format (t.id or '<unclassed>', List.concat (t.restrictions, ', '))
            end
          elseif (Ast.isNonTerminal (t)) then

            --- @cast t NonTerminalSymbol
            if (inline == true) then

              return assert (t.id, 'non-terminal symbols must be classed')
            else

              local lines = List { }
              local nons = List (Set.values (extractNons (t)))

              for _, non in ipairs (nons) do

                for _, production in ipairs (non.productions or {}) do

                  --- @cast production Operand
                  local rul = ast_mt.__tostring (non, true)
                  local op1 = ast_mt.__tostring (production, true)

                  lines = List.append (lines, ('%s -> %s'):format (rul, op1))
                end
              end
              return List.concat (lines, '\n')
            end
          else

            error (('unknown AST node: %s'):format (pretty.write (t)))
          end
        end,
      }

    grammar =
      {
        --- @type fun(self: Grammar, symbol: string | Symbol, assoc: Associativity): Symbol
        ---
        associative = function (self, symbol, a)

          local node = Grammar._node (self, symbol)
          local assoc = utils.assert_arg (2, a, 'string')
            node.associativity = assoc
          return node
        end,

        --- @type fun(self: Grammar, value: string): Symbol
        ---
        literal = function (self, value)

          return Grammar._node (self, utils.assert_string (1, value))
        end,

        --- @type fun(self: Grammar, class: string): Symbol
        ---
        nonterminal = function (self, class)

          return Grammar._add (self, class, Ast.newSymbol (class, false))
        end,

        --- @type fun(self: Grammar, symbol: string | Symbol, value: integer): Symbol
        ---
        precedence = function (self, symbol, p)

          local node = Grammar._node (self, symbol)
          local precedence = utils.assert_arg (2, p, 'number', function (e) return e == math.floor (e) end, 'not an integer')
            node.precedence = precedence
          return node
        end,

        --- @type fun(self: Grammar, symbol: string | Symbol, operand: Operand): Symbol, Operand
        ---
        produce = function (self, symbol, o)

          local n = Grammar._node (self, symbol)
          local node = utils.assert_arg (1, n, 'table', Ast.isNonTerminal, 'not a non-terminal symbol')
          local operand = utils.assert_arg (1, o, 'table', Ast.isOperand, 'not an operand')

          --- @cast node NonTerminalSymbol
          --- @cast operand Operand
            List.append (node.productions, operand)
          return node, operand
        end,

        --- @type fun(self: Grammar, symbol: string | Symbol, values: string []): Symbol
        ---
        restrict = function (self, symbol, v)

          local n = Grammar._node (self, symbol)
          local node = utils.assert_arg (1, n, 'table', Ast.isTerminal, 'not a terminal symbol')
          local values = utils.assert_arg (2, v, 'table')

          --- @cast node TerminalSymbol
            node.restrictions = Set.union (node.restrictions, values)
          return node
        end,

        --- @type fun(self: Grammar, class: string): Symbol
        ---
        symbol = function (self, class) return Grammar._get (self, class) end,

        --- @type fun(self: Grammar, id: string): Symbol
        ---
        token = function (self, id) return Grammar._add (self, id, Ast.new ('symbol', id, true)) end,

        symbols = {},
      }

    return setmetatable (grammar, grammar_mt)
  end
return Grammar
end
