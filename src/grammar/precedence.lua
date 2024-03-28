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
local Precedence = { }

do
  ---
  --- Base precedence
  ---
  Precedence.base = 0

  ---
  --- Calculate precedence for symbol
  ---
  --- @param p integer # precedence
  --- @param t? Associativity # associativity
  --- @return integer # precedence
  ---
  function Precedence.assoc (p, t) return t == nil and p or Precedence[t] (p) end

  ---
  --- Increments precedence
  ---
  --- @param p integer # precedence
  --- @return integer # precedence
  ---
  function Precedence.inc (p) return p + 10 end

  ---
  --- Calculate precedence for left-associative symbol
  ---
  --- @param p integer # precedence
  --- @return integer # precedence
  ---
  function Precedence.left (p) return p end

  ---
  --- Calculate precedence for symbol and increments
  ---
  --- @param p integer # precedence
  --- @param t? Associativity # associativity
  --- @return integer # precedence
  --- @return integer # next precedence
  ---
  function Precedence.next (p, t) return Precedence.assoc (p, t), Precedence.inc (p) end

  ---
  --- Calculate precedence for right-associative symbol
  ---
  --- @param p integer # precedence
  --- @return integer # precedence
  ---
  function Precedence.right (p) return p + 5 end

return Precedence
end
