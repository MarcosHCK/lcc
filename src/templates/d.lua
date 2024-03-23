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

--- @module 'templates.grammar'
local Grammar

--- @alias MainFunc fun(tout: file*)

---
--- Template entry point (expressions are valid outside but code generation
--- is restricted to this function's scope)
--- @type MainFunc
---
main = nil

---
--- Prints some output to the template output stream (like file:write (...))
--- Note: is only defined inside template's main function
---
--- @param ... string
---
function _ (...) end

---
--- Makes symbol associative
---
--- @param symbol string | Symbol
--- @param assoc Associativity
---
function associative (symbol, assoc) end

---
--- See io.stderr:write (...)
--- @param ... string
---
function fail (...) end

---
--- Sets the initial non-terminal ( S symbol in BNF theory )
---
--- @param symbol Symbol
--- @return Symbol
---
function initial (symbol) end

---
--- Wraps a literal value to allow operations over it
--- Note: due to templates limitations there is no ways to operate directly a
--- production as RULE = 'a' + 'b' ( SOME -> a | b in BNF ), so the left literal
--- should be wrapped using literal
---
--- @param value string
--- @return Symbol
---
function literal (value) end

---
--- Creates a non-terminal symbol
---
--- @return Symbol
---
function nonterminal () end

---
--- Shorthand for a associative and precedence over the same symbol
---
--- @param symbol string | Symbol
--- @param precedence integer
--- @param assoc Associativity
---
function operator (symbol, precedence, assoc) end

---
--- Sets symbol precedence (only meaningful if it is also associative)
---
--- @param symbol string | Symbol
--- @param precedence integer
---
function precedence (symbol, precedence, assoc) end

---
--- Creates a named token (one which has a token class associated to it)
---
--- @param ... string
--- @return Symbol
---
function token (...) end
