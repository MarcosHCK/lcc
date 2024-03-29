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

--- @alias Action string
--- @alias ActionType 'accept' | 'reduce' | 'shift'

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

      local a1, a2 = ...

      a1 = utils.assert_arg (2, a1, 'number', function (n)

        return n == math.floor (n)
      end, 'not an integer')

      if (type == 'reduce') then return 'r' .. tostring (a1) .. (not a2 and '' or '.' .. a2)
      elseif (type == 'shift') then return 's' .. tostring (a1)
      else utils.assert_arg (1, type, 'string', function () return false end, 'not an action type')
      end
    end

    error ('WTF?')
  end

  ---
  --- @param from string
  --- @return ActionType action_type
  --- @return integer? action_target
  --- @return integer? action_target2
  ---
  function Action.unserialize (from)

    utils.assert_string (1, from)

    local type, tail = types [from:sub (1, 1)], from:sub (2)

    if (type == 'accept') then

      assert (tail == 'cc', 'invalid action \'' .. from .. '\'')
      return type
    elseif (type == 'reduce') then

      local arg1, arg2 = tail:match ('^([0-9]+)%.([0-9]+)$')

      if (not arg1) then

        arg1 = tail:match ('^([0-9]+)$')
        arg1 = assert (tonumber (arg1), 'invalid action \'' .. from .. '\'')
      else

        arg1 = assert (tonumber (arg1), 'invalid action \'' .. from .. '\'')
        arg2 = assert (tonumber (arg2), 'invalid action \'' .. from .. '\'')
      end
      return type, arg1, arg2
    elseif (type == 'shift') then

      local arg1 = assert (tonumber (tail), 'invalid action \'' .. from .. '\'')
      return type, arg1
    else

      error ('unknown action type prefix \'' .. from:sub (1, 1) .. '\'')
    end
  end
return Action
end
