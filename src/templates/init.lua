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
local canonicalize = require ('grammar.canonicalize')
local compat = require ('pl.compat')
local Grammar = require ('grammar')
local pathutil = require ('pl.path')
local Precedence = require ('grammar.precedence')
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

  --- @param from string
  --- @param name string
  --- @param iffailed string
  --- @return any
  ---
  local function pick (from, name, iffailed)

    local base = utils.assert_string (1, name)
    local path = pathutil.normpath (base)

    assert (not path:match ('[%/]'), string.format (iffailed, name))

    local success, result = pcall (require, ('%s.%s'):format (from, path))
    local picked = assert (success and result, string.format (iffailed, name))
    return picked
  end

  --- @param stdout file*
  --- @param ... any
  ---
  local function printf (stdout, ...)

    local pre = ''

    for _, arg in ipairs ({...}) do

      stdout:write (tostring (arg), pre)
      pre = '\t'
    end

    stdout:write ('\n')
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

    --- @type Algorithm
    local algorithm
    --- @type Generator
    local generator

    local global
    local global_mt

    local tmain
    local first = true

    local grammar = Grammar.new ()
    local ruler_mt = { __name = 'Ruler', __tostring = function () return 'RulerCreator' end }
    local token_mt = { __name = 'Token', __tostring = function () return 'TokenCreator' end }

    global =
      {
        --- @type fun(name: string)
        ---
        algorithm = function (name)

          assert (not algorithm, 'parser algorithm was already defined')
          algorithm = pick ('algorithms', name, 'invalid algorithm \'%s\'')
        end,

        --- @type fun(...: any): any
        ---
        fail = function (...) return printf (io.stderr, ...) end,

        --- @type fun(name: string)
        ---
        generator = function (name)

          assert (not generator, 'parser generator was already defined')
          generator = pick ('generators', name, 'invalid generator \'%s\'')
        end,

        --- @type fun(a: NonTerminalSymbol): Symbol
        ---
        initial = function (a)

          if (first) then

            first = not grammar:initial (a)
          else

            local initials = Grammar._filter (grammar, function (_, e)

              --- @cast e NonTerminalSymbol
              return not e.terminal and e.initial == true
            end)

            for _, symbol in pairs (initials) do symbol.initial = false end
            grammar:initial (a)
          end
          return a
        end,

        --- @type fun(a: string | Symbol): Symbol
        left = function (a) return grammar:associative (a, 'left') end,

        --- @type fun(a: string): Symbol
        ---
        literal = function (a) return grammar:assert (1, a) end,

        --- @type fun(a: string | Symbol): Symbol
        right = function (a) return grammar:associative (a, 'right') end,

        --- @type fun(...: string): Token
        ---
        token = function (...)

          for i, arg in ipairs ({...}) do

            utils.assert_string (i, arg)
          end
          return setmetatable ({...}, token_mt)
        end,
      }

    global_mt =
      {
        __index = function (_, k) return _G[k] or grammar:symbol (k) or grammar:nonterminal (k) end,

        __name = 'TemplateGlobal',

        __newindex = function (t, key, value)

          if (utils.is_type (value, ruler_mt)) then grammar:nonterminal (key)
          elseif (utils.is_type (value, token_mt)) then grammar:restrict (grammar:token (key), value)
          elseif (grammar:check (value)) then grammar:produce (grammar:symbol (key) or grammar:nonterminal (key), grammar:operand (2, value))
            if (first) then

              first = false
              ---@diagnostic disable-next-line: param-type-mismatch
              grammar:initial (grammar:symbol (key))
            end
          elseif (key == 'main') then tmain = assert (tmain == nil and assert (type (value) == 'function' and value, 'invalid template main'), 'redefining template main')
          else rawset (t, key, value)
          end
        end
      }

    global._G = global

    local chunk, reason = compat.load (source, chunkname, mode, setmetatable (global, global_mt))

    if (not chunk) then return nil, reason
    else

      local success, result = xpcall (chunk, function (msg)

        return debug.traceback (msg)
      end)

      if (not success) then return nil, result
      else

        local env = setmetatable ({}, { __index = global })
        local main = tmain or (function ()

          assert (algorithm ~= nil, 'parser algorithm was not defined')
          assert (generator ~= nil, 'parser generator was not defined')

          local table = env.algorithm.emit (env.grammar)
          local parser = env.generator.emit (table)
          env._ (parser)
        end)

        env._G = env
        env.algorithm = algorithm
        env.generator = generator
        env.grammar = canonicalize (grammar)

        return function (stdout)

          env._ = function (...) printf (stdout, ...) end
          return (compat.setfenv (main, env)) (stdout)
        end
      end
    end
  end
return templates
end
