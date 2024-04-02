# Copyright 2021-2025 MarcosHCK
# This file is part of lcc.
#
# lcc is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# lcc is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with lcc.  If not, see <http://www.gnu.org/licenses/>.
#
from collections import namedtuple
from typing import Any, Callable, Dict, Iterable, Tuple

Action = namedtuple ('Action', [ 'type', 'target' ])
Capture = Callable [[Tuple], Any]
Reduce = namedtuple ('Reduce', [ 'lhs', 'nrhs' ])

##
##  for line in prolog do
##
f"line"
##  end
##
def Parser (stream: Iterable[Token]):

  actions: Dict[int, Dict[int, Action]] = {
##
##  for n in items:iter () do
##
##    if (Map.len (actions [n]) > 0) then
##
    f"n" : {
##
##      for i, id, symbol in seq.enum (OrderedMap.iter (parser.symbols)) do
##
##        if (actions [n] [symbol]) then
##
##          local serialized = actions [n] [symbol]
##          local type, target = Action.unserialize (serialized)
##
##          if (type == 'accept') then
##
      f"i" : Action (type = 'f"type"', target = None),
##          elseif (type == 'shift') then
##
      f"i" : Action (type = 'f"type"', target = f"target"),
##          elseif (type == 'reduce') then
##
##            local rule = items [n] [target]
##            local base, nprod = utils.unpack (rule)
##
      f"i" : Action (type = 'f"type"', target = Reduce (lhs = f"backref [base]", nrhs = f"rule:size ()")),
##          else error ('WTF?')
##          end
##        end
##     end
    },
##    end
##  end
  }

  def capture0 (args: Tuple, first: Token) -> Any:

    if (args.__len__ () == 1):

      return args [0]

    return args
##
##  local function capturename (index)
##
##    return 'capture' .. tostring (index)
##  end
##
##  for capture, i in captures:iter () do
##

  def f"capturename (i)" (args: Tuple, first: Token) -> Any:

##    for line in capture do
##
    f"line"
##    end
##  end
##

  captures: Dict [int, Dict [int, Capture]] = {
##
##  local function hascaptures (n)
##
##    for _, symbol in OrderedMap.iter (parser.symbols) do
##
##      if (actions [n] [symbol]) then
##
##        local serialized = actions [n] [symbol]
##        local type, ds, capture = Action.unserialize (serialized)
##
##        if (type == 'reduce' and capture ~= nil) then
##
##          return true
##        end
##      end
##    end
##    return false
##  end
##
##  for n, item in items:iter () do
##
##    if (hascaptures (n)) then
##
    f"n" : {
##
##      for i, id, symbol in seq.enum (OrderedMap.iter (parser.symbols)) do
##
##        if (actions [n] [symbol]) then
##
##          local serialized = actions [n] [symbol]
##          local type, _, capture = Action.unserialize (serialized)
##
##          if (type == 'reduce' and capture ~= nil) then
##
      f"i" : f"capturename (capture)",
##          end
##        end
##      end
    },
##    end
##  end
##
  }

  gotos: Dict [int, Dict [str, int]] = {
##
##  for n in items:iter () do
##
##    if (Map.len (gotos [n]) > 0) then
##
    f"n" : {
##
##      for i, _, symbol in seq.enum (OrderedMap.iter (parser.symbols)) do
##  
##        if (gotos [n] [symbol]) then
##  
      f"i" : f"gotos [n] [symbol]",
##        end
##      end
    },
##    end
##  end
  }

  def Symbol (token: Token) -> int:

    symbols_by_class = {
##
##  for k, v in classes (symbols) do
##
      'f"k"' : f"v",
##  end
    }

    symbols_by_lexeme = {
##
##  for k, v in restrictions (symbols) do
##
      'f"k"' : f"v",
##  end
    }

    k = symbols_by_class.get (token.type, symbols_by_lexeme.get (token.value))

    if k != None: return k
    else: raise Exception (f'unknown {token.type} token ({token.value})')

  def Expected (state: int) -> str:

    expected = [ ]

    symbols_by_index = {
##
##  for i, _, symbol in seq.enum (OrderedMap.iter (parser.symbols)) do
##
##    if (symbol.terminal and symbol.id ~= nil) then
##
        f"i": [ 'f"symbol.id"' ],
##    elseif (symbol.terminal) then
##
        f"i": [ f"table.concat (List.map (symbol.restrictions, function (e) return ('\'%s\''):format (escape (e)) end), ',')" ],
##    end
##  end
##
    }

    for symbol in actions [state].keys ():

      expected.extend (list (map (lambda a: f'\'{a}\'', symbols_by_index [symbol])))

    if (len (expected) < 2):

      return expected [0]
    elif (len (expected) < 3):

      return f'{expected [0]} or {expected [1]}'
    else:

      return ', '.join (expected [:-2]) + f' or {expected [-1]}'

  stack = [ 1 ]

  for token in stream:

    symbol = Symbol (token)

    while True:

      state = stack.pop ()
      action = actions [state].get (symbol)

      if (action == None):

        raise ParserException (token, Expected (state))

      elif (action.type == 'accept'):

        return stack.pop ()

      elif (action.type == 'shift'):

        stack.append (state)
        stack.append (token)
        stack.append (action.target)
        break

      elif (action.type == 'reduce'):

        capture = capture0
        things = []

        if (captures.get (state) != None):

          capture = captures [state].get (symbol, capture0)

        for i in range (action.target.nrhs):

          thing = stack.pop ()
          state = stack.pop ()
          things.append (thing)

        things.reverse ()

        stack.append (state)
        stack.append (capture (tuple (things), None if len (things) == 0 else things [0]))
        stack.append (gotos [state] [action.target.lhs])

  raise ParserException (Token (type = 'EOF', value = None), Expected (stack [-1]))

class ParserException (Exception):

  column: int
  line: int
  token: Token

  def __init__ (self, token, expected) -> None:

    super ().__init__ ()

    self.column = token.column
    self.expected = expected
    self.line = token.line
    self.token = token

  def __str__ (self) -> str:

    return f'{self.line}: {self.column}: unexpected token \'{self.token.type}\', expected {self.expected}'
