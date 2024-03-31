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

Token = namedtuple ('Token', [ 'column', 'line', 'type', 'value' ])

class Node:

  pass

class BinaryOperator (Node):

  def __init__ (self, operator: str, operand1: Node, operand2: Node):

    super ().__init__ ()

    self.operand1 = operand1
    self.operand2 = operand2
    self.operator = operator

  def __str__ (self):

    return f'({self.operand1.__str__ ()} {self.operator} {self.operand2.__str__ ()})'

class Immediate (Node):

  def __init__ (self, value: str):

    super ().__init__ ()

    self.value = value

  def __str__ (self):

    return self.value.__str__ ()

class Invoke (Node):

  def __init__ (self, funcname: str, argument: Node):

    super ().__init__ ()

    self.argument = argument
    self.funcname = funcname

  def __str__ (self):

    return f'{self.funcname.__str__ ()} ({self.argument.__str__ ()})'

def Lexer (func: str):

  yield Token (1, 1, 'ident', func)
  yield Token (1, 1, None, '(')
  yield Token (1, 1, 'number', '1')
  yield Token (1, 1, None, '-')
  yield Token (1, 1, 'number', '5')
  yield Token (1, 1, None, '*')
  yield Token (1, 1, None, '(')
  yield Token (1, 1, 'number', '6')
  yield Token (1, 1, None, '+')
  yield Token (1, 1, 'number', '2')
  yield Token (1, 1, None, '*')
  yield Token (1, 1, 'number', '3')
  yield Token (1, 1, None, ')')
  yield Token (1, 1, None, ')')
  yield Token (1, 1, 'EOF', None)

def Dump (tokens: Iterable[Token]):

  for token in tokens:

    if (token.type == 'EOF'):
      return

    yield token.value
