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
local OrderedMap = require ('pl.OrderedMap')
local utils = require ('pl.utils')

--- @class Captures: table<TriggerFunc, integer>
--- @field private map OrderedMap<fun(), integer>
--- @field private next integer
local Captures = { }

do

  local meta =
    {
      __index = function (_, k) return Captures[k] end,
      __name = 'Captures',
    }

  function Captures.is (arg) return utils.is_type (arg, meta) end

  ---
  --- @param key TriggerFunc
  --- @return integer
  ---
  function Captures:index (key)

    utils.assert_arg (0, self, 'table', Captures.is, 'not a Capture instance')
    utils.assert_arg (1, key, 'function')

    if (not self.map [key]) then

      self.map [key], self.next = self.next, self.next + 1
    end
    return self.map [key]
  end

  --- @return fun (): TriggerFunc?, integer?
  ---
  function Captures:iter ()

    utils.assert_arg (0, self, 'table', Captures.is, 'not a Capture instance')
    return OrderedMap.iter (self.map)
  end

  ---
  --- @return Captures
  ---
  function Captures.new ()

    return setmetatable ({ map = OrderedMap { }, next = 1 }, meta)
  end
return Captures
end
