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
--- @module 'templates.d'
local d
local compat = require ('pl.compat')
local Grammar = require ('templates.grammar')
local templates = {}
local utils = require ('pl.utils')

--- @class Packed<T>: { n: integer, [integer]: T }
--- @class Token: { [integer]: string }

do

  ---
  --- Similar to package.path for templates.include
  --- @type string
  ---
  templates.path = '?.lua'

  ---
  --- @param global table
  --- @param chunk function | nil
  --- @param reason? string
  --- @return nil | MainFunc template
  --- @return (string | table)? reasonOrSymbols
  ---
  local function loadCapture (global, chunk, reason)

    if (not chunk) then

      return nil, reason
    else

      local success, result = xpcall (chunk, function (msg)

        return debug.traceback (msg)
      end)

      if (not success) then return nil, result
      elseif (not global.main) then return function () end
      else

        return function (stdout)

          local env = setmetatable ({}, { __index = global })
          local fun = compat.setfenv (global.main, env)

          env._G = env
          env['_'] = function (...) stdout:write (..., '\n') end
          return (compat.setfenv (global.main, env)) (stdout)
        end
      end
    end
  end

  ---
  --- Compiles a template
  ---
  --- @param source string | function
  --- @param chunkname? string
  --- @param mode? string
  --- @return MainFunc | nil
  --- @return (string | table)? reasonOrSymbols
  ---
  function templates.compile (source, chunkname, mode)

    if (type (source) ~= 'string' and type (source) ~= 'function') then

      utils.assert_arg (1, source, 'string')
    end

    if (chunkname ~= nil) then utils.assert_arg (2, chunkname, 'string') end
    if (mode ~= nil) then utils.assert_arg (3, mode, 'string') end

    local global
    local global_mt

    local grammar = Grammar.new ()

    local ruler_mt = { __name = 'Ruler', __tostring = function () return 'RulerCreator' end }
    local token_mt = { __name = 'Token', __tostring = function () return 'TokenCreator' end }

    global_mt =
      {
        __index = function (_, k) return _G[k] or grammar:symbol (k) end,

        __name = 'TemplateGlobal',

        __newindex = function (t, key, value)

          if (utils.is_type (value, ruler_mt)) then grammar:nonterminal (key)
          elseif (utils.is_type (value, token_mt)) then grammar:restrict (grammar:token (key), value)
          elseif (grammar:symbol (key) ~= nil) then grammar:produce (grammar:symbol (key), value)
          else rawset (t, key, value)
          end
        end
      }

    global =
      {
        --- @type fun(symbol: string | Symbol, assoc: Associativity)
        ---
        associative = function (symbol, assoc) grammar:associative (symbol, assoc) end,

        --- @type fun(...: string): any
        ---
        fail = function (...) return assert (io.stderr:write (..., '\n')) end,

        --- @type fun(a: string): Symbol
        ---
        literal = function (a) return grammar:literal (a) end,

        --- @type fun(a: string): Symbol
        ---
        nonterminal = function (a) return setmetatable ({ }, ruler_mt) end,

        --- @type fun(a: string | Symbol, precedence: integer, assoc: Associativity)
        ---
        operator = function (a, precedence, assoc) grammar:associative (grammar:precedence (a, precedence), assoc) end,

        --- @type fun(a: string | Symbol, precedence: integer)
        ---
        precedence = function (symbol, precedence) grammar:precedence (symbol, precedence) end,

        --- @type fun(...: string): Token
        ---
        token = function (...)

          for i, arg in ipairs ({...}) do

            utils.assert_string (i, arg)
          end
          return setmetatable ({...}, token_mt)
        end,
      }

    global._G = setmetatable (global, global_mt)

    return loadCapture (global, compat.load (source, chunkname, mode, global))
  end
return templates
end
