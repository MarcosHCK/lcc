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

local EOF = 'EOF'
local EPSILON = 'EPSILON' -- should be ε

--- @class List<T>: table<integer, T>
--- @field append fun(self: List, item: any)
--- @field extend fun(self: List, other: List)
--- @field index fun(self: List, item: any): integer?
--- @field len fun(self: List): integer
--- @field concat fun(self: List, sep?: string): string
--- @field pop fun(self: List): any
local List = require ('pl.List')

--- @class Map<K, V>: table<K, V>
--- @field iter fun (self: Map): (fun (): any?, any?)
--- @field set fun (self: Map, key: any, value: any)
local Map = require ('pl.Map')

local Precedence = require ('grammar.precedence')

--- @class OrderedMap<K, V>: Map<K, V>
--- @field private _keys any[]
--- @field public keys fun(self: OrderedMap): any[]
--- @field public sort fun(self: OrderedMap, func: Comparer)
local OrderedMap = require ('pl.OrderedMap')

--- @class Set<T>: table<T, boolean>
--- @field len fun(self: Set): integer
--- @field union fun(a: Set, b: Set): Set
--- @field values fun(self: Set): List
local Set = require ('pl.Set')

--- @class Grammar
--- @field public assert fun (self: Grammar, argn: integer, arg: any, valid?: Validator, msg?: string): Symbol
--- @field public associative fun (self: Grammar, symbol: string | Symbol, assoc: Associativity): Symbol
--- @field public check fun (self: Grammar, arg: any): boolean
--- @field public initial fun (self: Grammar, symbol?: NonTerminalSymbol): NonTerminalSymbol?
--- @field private nextautomate integer
--- @field public nonterminal fun (self: Grammar, class: string): NonTerminalSymbol
--- @field public operand fun (self: Grammar, argn: integer, arg: any, valid?: Validator, msg?: string): Operand
--- @field public precedence fun (self: Grammar, symbol: string | Symbol, value: integer): Symbol
--- @field public produce fun (self: Grammar, symbol: Symbol, operand: Operand): Symbol, Operand
--- @field public restrict fun (self: Grammar, symbol: TerminalSymbol, values: string []): TerminalSymbol
--- @field public symbol fun (self: Grammar, class: string): Symbol
--- @field private symbols OrderedMap<string, Symbol>
--- @field public token fun (self: Grammar, id: string): TerminalSymbol
local Grammar = { }

--- @alias Comparer fun(a: any, b: any): boolean
--- @alias Validator fun(arg: any): boolean

do

  local grammar_mt =
    {
      __index = Grammar,

      __name = 'Grammar',

      --- @type fun (self: Grammar): string
      __tostring = function (self) return tostring (self:initial ()) end,
    }

  ---
  --- @return NonTerminalSymbol
  ---
  function Grammar:_automate ()

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

  ---
  --- @param productions? boolean # copy productions too
  --- @return self
  ---
  function Grammar:_copy (productions)

    local eof = self.symbols [EOF]
    local epsilon = self.symbols [EPSILON]
    local specials = List { eof, epsilon }
    local order = Map { }
    local out = Grammar.new ()
    local pos = 1

    for id, symbol in OrderedMap.iter (self.symbols) do

      --- @cast id string
      --- @cast symbol Symbol
      local sym

      order [id], pos = pos, pos + 1

      if (not symbol.terminal) then

        --- @cast symbol NonTerminalSymbol
        sym = out:nonterminal (id)
        sym.initial = symbol.initial
      elseif (List.index (specials, symbol) == nil) then

        --- @cast symbol TerminalSymbol

        if (symbol.id ~= nil) then

          sym = out:token (symbol.id)
        else

          sym = out:assert (0, id:match ('\'([^\']+)\''))
        end

        --- @cast sym TerminalSymbol
        sym = out:restrict (sym, symbol.restrictions)
      end if (sym ~= nil) then

        sym.associativity = symbol.associativity
        sym.precedence = symbol.precedence
      end
    end

    OrderedMap.sort (out.symbols, function (a, b) return order[a] < order[b] end)

    if (productions == true) then

      local inner
      local lookup = {}

      for id, symbol in OrderedMap.iter (self.symbols) do

        --- @cast id string
        --- @cast symbol Symbol

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
        elseif (production.type == 'operator' and production.kind == '$') then

          --- @cast production TriggerOperator
          return inner (production.operand1) / production.callback
        else

          error (('unknown AST node %s'):format (production))
        end
      end

      for _, symbol in OrderedMap.iter (self.symbols) do

        --- @cast symbol Symbol

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

  ---
  --- @param fn fun(key: string, symbol: Symbol): boolean
  --- @return OrderedMap<string, Symbol>
  ---
  function Grammar:_filter (fn)

    local filtered = OrderedMap { }

    for key, symbol in OrderedMap.iter (self.symbols) do

      --- @cast key string
      --- @cast symbol Symbol

      if (fn (key, symbol) == true) then

        filtered [key] = symbol
      end
    end
  return filtered
  end

  ---
  --- @param id string
  --- @return Symbol
  ---
  function Grammar:_get (id)

    return self.symbols [utils.assert_string (1, id)]
  end

  ---
  --- @param cmp fun(a: Symbol, b: Symbol): boolean
  ---
  function Grammar:_sort (cmp)

    OrderedMap.sort (self.symbols, cmp)
  end

  ---
  --- Symbol for EOF terminal (end of file (or stream))
  ---
  --- @type string
  ---
  Grammar.EOF = EOF

  ---
  --- Symbol for ε terminal (empty string terminal)
  ---
  --- @type string
  ---
  Grammar.EPSILON = EPSILON

  ---
  --- Creates a Symbols instance
  ---
  --- @return Grammar
  ---
  function Grammar.new ()

    local ast_mt

    local grammar
    local order = 0
    local precedence = Precedence.inc (Precedence.base)

    --- @module 'grammar.ast'
    local Ast = { }

    ---
    --- @param self Grammar
    --- @param name string
    --- @param symbol Symbol
    --- @return Symbol
    ---
    local function add (self, name, symbol)

      ---@diagnostic disable-next-line: invisible
      assert (not self.symbols [name], ('redefining symbol \'%s\''):format (name))

      ---@diagnostic disable-next-line: invisible
      self.symbols [name] = symbol

      if (not symbol.terminal) then

        symbol.precedence, order = order, order + 1
      else

        symbol.precedence, precedence = Precedence.next (precedence)
      end
      return symbol
    end

    ---
    --- @param o Operand
    --- @return Set<TerminalSymbol>
    ---
    local function collectNons (o)

      if (Ast.isNonTerminal (o)) then return Set { o }
      elseif (Ast.isOperator (o)) then

        --- @cast o BinaryOperator
        return collectNons (o.operand1) + (collectNons (o.operand2 or {}))
      end
      return Set { }
    end

    ---
    --- @param o Operand
    --- @return Set<TerminalSymbol>
    ---
    local function extractNons (o)

      local nexts = List { o }
      local nons = Set { o }

      while (List.len (nexts) > 0) do

        for _, p in ipairs (List.pop (nexts).productions or { }) do

          local news = Set.union (nons, collectNons (p))

          nexts = List.extend (nexts, Set.values (news - nons))
          nons = news
        end
      end
      return Set.values (nons)
    end

    --- @type fun(): EofSymbol
    ---
    Ast.eof = function ()

      local symbol = Ast.newSymbol (EOF, true)

      --- @cast symbol EofSymbol
        symbol.eof = true
        symbol.precedence = math.huge
      return symbol
    end

    --- @type fun(): EpsilonSymbol
    ---
    Ast.epsilon = function ()

      local symbol = Ast.newSymbol (EPSILON, true)

      --- @cast symbol EpsilonSymbol
        symbol.epsilon = true
        symbol.precedence = math.huge
      return symbol
    end

    --- @type fun(o: Operand, terminal?: boolean): Symbol?
    ---
    Ast.first = function (operand, terminal)

      if (operand.type == 'symbol') then

        --- @cast operand Symbol
        return (terminal == nil or operand.terminal) and nil or operand
      elseif (operand.type == 'operator') then

        --- @cast operand BinaryOperator
        return Ast.first (operand.operand1) or (not operand.operand2 and nil or Ast.first (operand.operand2))
      else

        error ('unknown AST node ' .. tostring (operand))
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

    --- @type fun(kind: OperatorKind, op1: Operand, op2?: Operand): Operator
    Ast.newOperator = func.bind1 (Ast.new, 'operator')
    --- @type fun(id?: string, terminal: boolean): Symbol
    Ast.newSymbol = func.bind1 (Ast.new, 'symbol')
    --- @type fun(callback: TriggerFunc, op1: Operand): Operator
    Ast.newTrigger = func.bind1 (Ast.new, 'operator')

    ast_mt =
      {
        --- @type fun(a: Operand, b: string | Operand): Operator
        __add = function (a, b) return Ast.newOperator ('&', grammar:operand (1, a), grammar:operand (2, b)) end,
        --- @type fun(a: Operand, b: TriggerFunc): Operator
        __div = function (t, c) return Ast.newTrigger (utils.assert_arg (2, c, 'function'), grammar:operand (1, t)) end,
        --- @type fun(a: Operand, b: string | Operand): Operator
        __mul = function (a, b) return Ast.newOperator ('|', grammar:operand (1, a), grammar:operand (2, b)) end,

        __name = 'AST',

        --- @type fun(t: Operand, n: integer): Operator
        ---
        __pow = function (t, n)

          t = grammar:operand (1, t)
          n = utils.assert_arg (1, n, 'number', function (e) return e == math.floor (e) end, 'not an integer')

          if (n == 0) then return Ast.newOperator ('*', t)
          elseif (n == 1) then return Ast.newOperator ('+', t)
          elseif (n == -1) then return Ast.newOperator ('?', t)
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
              local nons = extractNons (t)

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
        ---
        --- @param self Grammar
        --- @param argn integer
        --- @param arg any
        --- @param valid? fun (arg: any): boolean
        --- @param msg? string
        --- @return Symbol
        ---
        assert = function (self, argn, arg, valid, msg)

          valid = valid or Ast.isSymbol
          msg = msg or 'not a symbol'

          if (type (arg) ~= 'string') then

            return utils.assert_arg (argn, arg, 'table', valid, msg)
          else

            local key = ('\'%s\''):format (arg)
            local sym = grammar.symbols [key]

            if (sym ~= nil) then return sym
            else

              local s = add (grammar, key, Ast.new ('symbol', nil, true))
              --- @cast s TerminalSymbol
              return self:assert (argn, grammar:restrict (s, { arg }), valid, msg)
            end
          end
        end,

        --- @type fun(self: Grammar, symbol: string | Symbol, assoc: Associativity): Symbol
        ---
        associative = function (self, symbol, a)

          local node = self:assert (1, symbol)
          node.associativity = utils.assert_arg (2, a, 'string')
          return node
        end,

        --- @type fun(self: Grammar, arg: any): boolean
        ---
        check = function (self, arg) return Ast.isAst (arg) end,

        --- @type fun(self: Grammar, symbol?: NonTerminalSymbol): NonTerminalSymbol?
        ---
        initial = function (self, symbol)

          local anothers = Grammar._filter (self, function (_, e)

            --- @cast e NonTerminalSymbol
            return Ast.isNonTerminal (e) and e.initial == true
          end)

          if (symbol == nil) then

            return anothers [OrderedMap.keys (anothers) [1]]
          else

            utils.assert_arg (1, symbol, 'table', Ast.isNonTerminal, 'not a non-terminal').initial = true
            return symbol
          end
        end,

        nextautomate = 1,

        --- @type fun(self: Grammar, class: string): NonTerminalSymbol
        ---
        nonterminal = function (self, class)

          --- @type NonTerminalSymbol
          return add (self, class, Ast.newSymbol (class, false))
        end,

        --- @type fun(self: Grammar, argn: integer, value: string | Operand | Symbol): Operand
        ---
        operand = function (self, argn, value)

          return self:assert (argn, value, Ast.isOperand, 'not an operand')
        end,

        --- @type fun(self: Grammar, symbol: string | Symbol, value: integer): Symbol
        ---
        precedence = function (self, symbol, p)

          local node = self:assert (1, symbol)
          node.precedence = utils.assert_arg (2, p, 'number', function (e) return e == math.floor (e) end, 'not an integer')
          return node
        end,

        --- @type fun(self: Grammar, symbol: Symbol, operand: Operand): Symbol, Operand
        ---
        produce = function (self, symbol, operand)

          local node = self:assert (1, symbol)
          utils.assert_arg (1, node, 'table', Ast.isNonTerminal, 'not a non-terminal symbol')
          utils.assert_arg (2, operand, 'table', Ast.isOperand, 'not an operand')

          --- @cast node NonTerminalSymbol
          --- @cast operand Operand
          List.append (node.productions, operand)
          return node, operand
        end,

        --- @type fun(self: Grammar, symbol: TerminalSymbol, values: string []): TerminalSymbol
        ---
        restrict = function (self, symbol, v)

          local n = self:assert (1, symbol)
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
          return add (self, id, Ast.new ('symbol', id, true))
        end,

        symbols = OrderedMap { [EOF] = Ast.eof (), [EPSILON] = Ast.epsilon () },
      }

    return setmetatable (grammar, grammar_mt)
  end
return Grammar
end
