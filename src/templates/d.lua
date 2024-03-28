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

--- @module 'grammar'
local Grammar

--- @alias MainFunc fun(tout: file*)

---
--- Template entry point (expressions are valid outside but code generation
--- is restricted to this function's scope)
--- @type MainFunc
---
main = nil

---
--- Grammar object generated from template
--- @type Grammar
---
grammar = nil

---
--- Prints some output to the template output stream (like file:write (...))
--- Note: is only defined inside template's main function
---
--- @param ... any
---
function _ (...) end

---
--- Use algorithm algo to generate the parser code
---
--- @param algo string
---
function algorithm (algo) end

---
--- See io.stderr:write (...)
--- @param ... any
---
function fail (...) end

---
--- Use code generator lang to generate the parser code
---
--- @param lang string
---
function generator (lang) end

---
--- Sets the initial non-terminal ( S symbol in BNF theory )
---
--- @param symbol Symbol
--- @return Symbol
---
function initial (symbol) end

---
--- Makes symbol left-associative
---
--- @param symbol string | Symbol
---
function left (symbol) end

---
--- Wraps a literal value to allow operations over it
--- Note: due to templates limitations there is no ways to operate directly a
--- production like RULE = 'a' + 'b' ( RULE -> a | b in BNF ), so the leftmost
--- literal must be wrapped using literal(resulting in
--- RULE = literal ('a') + 'b')
---
--- @param value string
--- @return Symbol
---
function literal (value) end

---
--- Makes symbol right-associative
---
--- @param symbol string | Symbol
---
function right (symbol) end

---
--- Creates a named token (one which has a token class associated to it)
---
--- @param ... string
--- @return Symbol
---
function token (...) end
