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
local app = require ('pl.app')
local argparse = require ('argparse')
local pathutils = require ('pl.path')

local function main (args)

  app.require_here ()

  local parser = argparse ()
  local templates = require ('templates')

  parser:argument ('input', 'Input lexer definition')
  parser:option ('-o --output', 'Emit code to file')

  local args = parser:parse ()
  local basedir = '.'
  local chunkname = '(stdin)'
  local input = io.stdin
  local output = io.stdout

  if (args.input and args.input ~= '-') then

    chunkname = pathutils.basename (args.input)
    input = assert (io.open (args.input, 'r'))

    do
      local anchor = pathutils.abspath (pathutils.dirname (args.input))
      local normal = pathutils.normpath (anchor)
      basedir = normal
    end
  end

  if (args.output and args.output ~= '-') then

    output = assert (io.open (args.output, 'w'))
  end

  local reader = function () local chunk = input:read ('*l'); return chunk and (chunk .. '\n') end
  local template = assert (templates.compile (reader, '=' .. chunkname, 't', basedir))

  input:close ()

  template (output)
  output:close ()
end

--- @diagnostic disable-next-line: undefined-global
main (args or arg or {...})
