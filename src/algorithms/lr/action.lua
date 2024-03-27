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
local Action = { }
local utils = require ('pl.utils')

--- @alias ActionType
--- | 'accept'
--- | 'reduce'
--- | 'shift'

local types = { a = 'accept', r = 'reduce', s = 'shift' }

do
  ---
  --- @param type ActionType
  --- @param ... any
  --- @return string
  ---
  function Action.serialize (type, ...)

    if (type == 'accept') then

      return 'acc'
    else

      local t = utils.assert_arg (2, (...), 'number', function (n)

        return n == math.floor (n)
      end, 'not an integer')

      if (type == 'reduce') then return 'r' .. tostring (t)
      elseif (type == 'shift') then return 's' .. tostring (t)
      else utils.assert_arg (1, type, 'string', function () return false end, 'not an action type')
      end
    end

    error ('WTF?')
  end

  ---
  --- @param serialized string
  --- @return ActionType action_type
  --- @return integer? action_target
  ---
  function Action.unserialize (serialized)

    local from = utils.assert_string (1, serialized)
    local type = assert (types [from:sub (1, 1)], 'unknown action type \'' .. from:sub (1, 1) .. '\'')
    local target = assert (types [from:sub (2)], 'invalid action \'' .. from .. '\'')
    assert (type ~= 'accept' or target == 'cc', 'invalid action \'' .. from .. '\'')
    return type, type == 'accept' and nil or tonumber (target)
  end
return Action
end
