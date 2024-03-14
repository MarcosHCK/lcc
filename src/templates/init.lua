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
local Ast = require ('templates.ast')
local compat = require ('pl.compat')
local func = require ('pl.func')
local OrderedMap = require ('pl.OrderedMap')
local tablex = require ('pl.tablex')
local templates = {}
local utils = require ('pl.utils')

--- @class Packed<T>: { n: integer, [integer]: T }

do

  ---
  --- Similar to package.path for templates.include
  --- @type string
  ---
  templates.path = '?.lua'

  --- @generic T
  --- @param level number
  --- @param cond? T
  --- @param message? any
  --- @param ... any
  --- @return T
  --- @return any ...
  local function levelAssert (level, cond, message, ...)

    if (not cond) then
      error (message or 'assertion failed!', level + 1)
    else
      return cond, message, ...
    end
  end

  ---
  --- @param global table
  --- @param symbols table
  --- @param chunk function | nil
  --- @param reason? string
  --- @return nil | MainFunc template
  --- @return (string | table)? reasonOrSymbols
  ---
  local function loadCapture (global, symbols, chunk, reason)

    if (not chunk) then

      return nil, reason
    else

      local success, result = pcall (chunk)

      if (not success) then

        return nil, result
      elseif (not global.main) then

        return function () end, symbols
      else

        return function (tout)

          local env = setmetatable ({ }, { __index = global })
          local fun = compat.setfenv (global.main, env)

          env._G = env
          env.symbols = tablex.deepcopy (symbols)
          env['_'] = function (...) tout:write (..., '\n') end
          return fun (tout)
        end, symbols
      end
    end
  end

  --- @param name string
  --- @param restrictions Packed<string>
  --- @return TerminalSymbol
  local function newTerminal (name, restrictions)

    local symbol = Ast.terminal (name)

    if (restrictions.n) then

      Ast.restrict (symbol, utils.unpack (restrictions))
    end
    return symbol
  end

  ---
  --- Includes a foreigh template into the current running one
  ---
  --- @param name string
  --- @return MainFunc | nil
  --- @return string? reason
  ---
  function templates.include (name)

    local file, reason = package.searchpath (name, templates.path)

    if (not file) then

      return nil, reason
    else

      local fp, reason = io.open (file, 'r')

      if (not fp) then

        return nil, reason
      else

        local reader = function () local chunk = fp:read ('*l'); return chunk and (chunk .. '\n') end
        local results = { templates.compile (reader, ('=%s'):format (name)) }
        fp:close ()

        return utils.unpack (results)
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

    local global = { }
    local symbols = OrderedMap { }
    local token_mt = { __name = 'token', __tostring = function () return 'token symbol' end }

    global._G = global

    ---
    --- @type fun(...: string)
    ---
    global.fail = function (...) assert (io.stderr:write (..., '\n')) end

    ---
    --- @type fun(name: string): MainFunc
    ---
    global.include = function (name)

      local main, newsymbols = levelAssert (2, templates.include (name))
      for k, symbol in OrderedMap.iter (newsymbols) do OrderedMap.set (symbols, k, symbol) end
      return main
    end

    ---
    --- @type fun(from: TerminalSymbol, ...: string): Packed<string>
    ---
    global.subset = function (from, ...)

      local args

      utils.assert_arg (1, from, 'table', Ast.isTerminal, 'not a terminal symbol', 3)

      args = global.token (...)
      ---@diagnostic disable-next-line: inject-field
      args.name = from.name
      return args
    end

    ---
    --- @type fun(...: string): Packed<string>
    ---
    global.token = function (...)

      local args = utils.pack (...)

      for i, arg in ipairs (args) do

        utils.assert_arg (i, arg, 'string', nil, nil, 3)
      end
      return setmetatable (args, token_mt)
    end

    setmetatable (global,
      {

        __index = function (_, k)

          return symbols[k] or _G[k] or (function ()

            local name = assert (type (k) == 'string' and k, 'symbol names should be an string')
            local symbol = Ast.nonTerminal (k)
            return OrderedMap.set (symbols, name, symbol) and symbol
          end) ()
        end,

        __newindex = function (t, key, value)

          if (key == 'main') then

            if (not rawget (t, key)) then

              rawset (t, key, value)
            else

              error ('redefining template main function')
            end
          else

            levelAssert (2, type (key) == 'string', 'symbol names should be an string')

            if (utils.is_type (value, token_mt)) then

              assert (symbols [key] == nil, 'redefining terminal symbol')
              OrderedMap.set (symbols, key, newTerminal (value.name or key, value))
            else

              local rule = t[key]
              levelAssert (2, Ast.isNonTerminal (rule), ('\'%s\' is not a non-terminal symbol'):format (key))
              levelAssert (2, Ast.isSymbol (value) or Ast.isOperator (value), ('\'%s\' is not symbol'):format (tostring (value)))
              Ast.produces (rule, value)
            end
          end
        end,
      })

    return loadCapture (global, symbols, compat.load (source, chunkname, mode, global))
  end
return templates
end
