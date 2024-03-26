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
local tablex = require ('pl.tablex')
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

--- @class Grammar: { [string]: Symbol }
--- @field public associative fun(self: Grammar, symbol: string | Symbol, assoc: Associativity): Symbol
--- @field public initial fun(self: Grammar, symbol: NonTerminalSymbol): NonTerminalSymbol
--- @field public literal fun(self: Grammar, value: string): TerminalSymbol
--- @field private nextautomate integer
--- @field public nonterminal fun(self: Grammar, class: string): NonTerminalSymbol
--- @field public precedence fun(self: Grammar, symbol: string | Symbol, value: integer): Symbol
--- @field public produce fun(self: Grammar, symbol: Symbol, operand: Operand): Symbol, Operand
--- @field public restrict fun(self: Grammar, symbol: TerminalSymbol, values: string []): TerminalSymbol
--- @field public symbol fun(self: Grammar, class: string): Symbol
--- @field private symbols table<string, Symbol>
--- @field public token fun(self: Grammar, id: string): TerminalSymbol
local Grammar = { }

--- @alias Associativity 'left' | 'right'
--- @alias AstType 'operator' | 'symbol'
--- @alias Operand Operator | Symbol
--- @alias OperatorKind '&' | '|' | '*' | '+' | '?' | '$'
--- @alias TriggerFunc fun()

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

--- @class EofSymbol: Symbol
--- @field public eof boolean

--- @class NonTerminalSymbol: Symbol
--- @field public initial? boolean
--- @field public productions? Operand[]

--- @class TerminalSymbol: Symbol
--- @field public restrictions? Set<string>

do

  local grammar_mt =
    {
      __index = function (self, k)

        return Grammar._get (self, k)
      end,

      __name = 'Grammar',

      __tostring = function (self)

        local initials = Grammar._filter (self, function (_, e)

          --- @cast e NonTerminalSymbol
          return e.type == 'symbol' and not e.terminal and e.initial == true
        end)

        if (tablex.size (initials) ~= 1) then

          return 'un-usabe grammar'
        else

          local initial = initials[tablex.keys (initials) [1]]
          local head = tostring (initial)
          return head
        end
      end,
    }

  --- @param self Grammar
  --- @return NonTerminalSymbol
  ---
  function Grammar._automate (self)

    local next

    for i = 1, math.huge, 1 do

      next = 'AUTOMATE' .. tostring (self.nextautomate + i - 1)

      if (not self.symbols[next]) then

        self.nextautomate = self.nextautomate + i

        --- @type NonTerminalSymbol
        return self:nonterminal (next)
      end
    end

    error ('WTF?')
  end

  --- @param self Grammar
  --- @param productions? boolean # copy productions too
  --- @return Grammar
  function Grammar._copy (self, productions)

    local eof = self.symbols.EOF
    local out = Grammar.new ()

    for id, symbol in pairs (self.symbols) do

      local sym

      if (not symbol.terminal) then

        --- @cast symbol NonTerminalSymbol
        sym = out:nonterminal (id)
        sym.initial = symbol.initial
      elseif (symbol ~= eof) then

        --- @cast symbol TerminalSymbol

        if (symbol.id ~= nil) then

          sym = out:token (symbol.id)
        else

          assert (tablex.size (symbol.restrictions) == 1)

          sym = out:literal (symbol.restrictions [1])
        end

        sym = out:restrict (sym, symbol.restrictions)
      end if (sym ~= nil) then

        sym.associativity = symbol.associativity
        sym.precedence = symbol.precedence
        sym.trigger = symbol.trigger
      end
    end

    if (productions == true) then

      local inner
      local lookup = {}

      for id, symbol in pairs (self.symbols) do

        lookup[symbol] = assert (Grammar._get (out, id))
      end

      --- @param production Operand
      --- @return Operand
      ---
      inner = function (production)

        if (production.type == 'symbol') then

          return lookup[production]
        elseif (production.type == 'operator' and production.kind == '&') then

          --- @cast production BinaryOperator
          local op1 = inner (production.operand1)
          local op2 = inner (production.operand2)
          return op1 + op2
        elseif (production.type == 'operator' and production.kind == '|') then

          --- @cast production BinaryOperator
          local op1 = inner (production.operand1)
          local op2 = inner (production.operand2)
          return op1 * op2
        elseif (production.type == 'operator' and production.kind == '*') then

          --- @cast production UnaryOperator
          return inner (production.operand1) ^ 0
        elseif (production.type == 'operator' and production.kind == '+') then

          --- @cast production UnaryOperator
          return inner (production.operand1) ^ 1
        elseif (production.type == 'operator' and production.kind == '?') then

          --- @cast production UnaryOperator
          return inner (production.operand1) ^ -1
        else

          error (('unknown AST node %s'):format (production))
        end
      end

      for _, symbol in pairs (self.symbols) do

        if (not symbol.terminal) then

          --- @cast symbol NonTerminalSymbol
          tablex.foreachi (symbol.productions, function (p)

            out:produce (lookup [symbol], inner (p))
          end)
        end
      end
    end
    return out
  end

  --- @param self Grammar
  --- @param fn fun(key: string, symbol: Symbol): boolean
  --- @return table<string, Symbol>
  ---
  function Grammar._filter (self, fn)

    local filtered = { }

    for key, symbol in pairs (self.symbols) do

      if (fn (key, symbol) == true) then

        filtered[key] = symbol
      end
    end
  return filtered
  end

  --- @param self Grammar
  --- @param id string
  --- @return Symbol
  ---
  function Grammar._get (self, id)

    return self.symbols [utils.assert_string (1, id)]
  end

  --- @param Ast Ast
  --- @param o Operand
  --- @return Set<TerminalSymbol>
  local function collectNons (Ast, o)

    if (Ast.isNonTerminal (o)) then return Set { o }
    elseif (Ast.isOperator (o)) then

      --- @cast o BinaryOperator
      return collectNons (Ast, o.operand1) + (collectNons (Ast, o.operand2 or {}))
    end
    return Set { }
  end

  --- @param Ast Ast
  --- @param o Operand
  --- @return Set<TerminalSymbol>
  local function extractNons (Ast, o)

    local nexts = List { o }
    local nons = Set { o }

    while (List.len (nexts) > 0) do

      for _, p in ipairs (List.pop (nexts).productions) do

        local news = Set.union (nons, collectNons (Ast, p))

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

    --- @class Ast
    --- @field public type AstType
    local Ast = { }

    --- @param self Grammar
    --- @param name string
    --- @param symbol Symbol
    --- @return Symbol
    ---
    local function _add (self, name, symbol)

      ---@diagnostic disable-next-line: invisible
      assert (not self.symbols[name], ('redefining symbol \'%s\''):format (name))

      ---@diagnostic disable-next-line: invisible
      self.symbols[name] = symbol
      return symbol
    end

    --- @param self Grammar
    --- @param value string | Symbol
    --- @return Symbol
    ---
    local function _node (self, value)

      if (type (value) ~= 'string') then

        return (utils.assert_arg (1, value, 'table', Ast.isSymbol, 'not a symbol'))
      else

        local id = ('\'%s\''):format (value)
        ---@diagnostic disable-next-line: invisible
        local sym = self.symbols[id]

        if (sym ~= nil) then return sym
        else

          local s = _add (self, id, Ast.new ('symbol', nil, true))

          --- @cast s TerminalSymbol
          return self:restrict (s, { value })
        end
      end
    end

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
    Ast.isEof = function (s) return Ast.is (s, 'symbol', true) and s.eof == true end
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

    --- @type fun(): EofSymbol
    Ast.eof = function ()

      local symbol = Ast.newSymbol ('EOF', true)

      --- @cast symbol EofSymbol
        symbol.eof = true
      return symbol
    end

    --- @type fun(o: string | Operand): Operand
    ---
    Ast.operand = function (o)

      if (type (o) == 'string') then

        return _node (grammar, o)
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

              if (t.kind == '&') then

                return ('(%s %s)'):format (op1, op2)
              elseif (t.kind == '|') then

                return ('(%s | %s)'):format (op1, op2)
              end
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

              return (assert (t.id, 'non-terminal symbols must be classed'))
            else

              local lines = List { }
              local nons = List (Set.values (extractNons (Ast, t)))

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
          end

          error (('unknown AST node: %s'):format (pretty.write (t)))
        end,
      }

    grammar =
      {
        --- @type fun(self: Grammar, symbol: string | Symbol, assoc: Associativity): Symbol
        ---
        associative = function (self, symbol, a)

          local node = _node (self, symbol)
          local assoc = utils.assert_arg (2, a, 'string')
            node.associativity = assoc
          return node
        end,

        --- @type fun(self: Grammar, symbol: NonTerminalSymbol): NonTerminalSymbol
        ---
        initial = function (self, symbol)

          local node = utils.assert_arg (1, symbol, 'table', Ast.isNonTerminal, 'not a non-terminal')
          local anothers = Grammar._filter (self, function (_, e)

            --- @cast e NonTerminalSymbol
            return Ast.isNonTerminal (e) and e.initial == true
          end)

          assert (tablex.size (anothers) == 0, 'initial symbol is already set')

          --- @cast node NonTerminalSymbol
            node.initial = true
          return node
        end,

        --- @type fun(self: Grammar, value: string): TerminalSymbol
        ---
        literal = function (self, value)

          --- @type TerminalSymbol
          return _node (self, utils.assert_string (1, value))
        end,

        --- @type fun(self: Grammar, class: string): NonTerminalSymbol
        ---
        nonterminal = function (self, class)

          --- @type NonTerminalSymbol
          return _add (self, class, Ast.newSymbol (class, false))
        end,

        --- @type fun(self: Grammar, symbol: string | Symbol, value: integer): Symbol
        ---
        precedence = function (self, symbol, p)

          local node = _node (self, symbol)
          local precedence = utils.assert_arg (2, p, 'number', function (e) return e == math.floor (e) end, 'not an integer')
            node.precedence = precedence
          return node
        end,

        --- @type fun(self: Grammar, symbol: Symbol, operand: Operand): Symbol, Operand
        ---
        produce = function (self, symbol, o)

          local n = _node (self, symbol)
          local node = utils.assert_arg (1, n, 'table', Ast.isNonTerminal, 'not a non-terminal symbol')
          local operand = utils.assert_arg (2, o, 'table', Ast.isOperand, 'not an operand')

          --- @cast node NonTerminalSymbol
          --- @cast operand Operand
            List.append (node.productions, operand)
          return node, operand
        end,

        --- @type fun(self: Grammar, symbol: TerminalSymbol, values: string []): TerminalSymbol
        ---
        restrict = function (self, symbol, v)

          local n = _node (self, symbol)
          local node = utils.assert_arg (1, n, 'table', Ast.isTerminal, 'not a terminal symbol')
          local values = utils.assert_arg (2, v, 'table')

          --- @cast node TerminalSymbol
            node.restrictions = Set.union (node.restrictions, values)
          return node
        end,

        --- @type fun(self: Grammar, class: string): Symbol
        ---
        symbol = function (self, class) return Grammar._get (self, class) end,

        --- @type fun(self: Grammar, id: string): TerminalSymbol
        ---
        token = function (self, id)

          --- @type TerminalSymbol
          return _add (self, id, Ast.new ('symbol', id, true))
        end,

        nextautomate = 1,
        symbols = { EOF = Ast.eof () },
      }

    return setmetatable (grammar, grammar_mt)
  end
return Grammar
end
