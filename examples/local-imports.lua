-------------------------------------------------------------------------------
-- 
-- A preprocessor that detects which basic modules and functions are being 
-- used, and adds local declarations at the top of the code. Somewhat useful
-- (at least for me) along a module declaration.
-- 
-- This is a very simplistic preprocessor: it simply searches for identifiers
-- with the same name as a basic module or function. This might yield false
-- positives, as a variable which happens to have the same name as a basic 
-- value will be incorrectly counted. Since the code generated here goes before
-- the code in the input, the program still runs normally, so the false 
-- positives don't affect its semantics.
-- 
-- A more complex analysis could be made, but it would be far easier to do it
-- with an AST.
-- 
-- Author: Humberto Anjos
-- 
-- $Id: local-imports.lua,v 1.2 2007/12/07 14:23:56 hanjos Exp $
-- 
-------------------------------------------------------------------------------

-- imported modules
local parser = require 'leg.parser'

local lpeg = require 'lpeg'

-- imported functions
local P = lpeg.P

-- HELPER CODE ------------------------

-- tables using the names of Lua's basic modules and functions as keys,
-- for easy searching
local basicModules, basicFunctions = {}, {}

-- populating the tables
for k, v in pairs(_G) do
  if type(v) == 'table' then
    basicModules[k] = true
  elseif type(v) == 'function' then
    basicFunctions[k] = true
  end
end

-- to pretty print the statements
local function maxNameLength(list)
  local max = 0
  
  for _, v in ipairs(list) do
    if max < #v then max = #v end
  end
  
  return max
end

local function buildStatements(list, type)
  local str, max = '-- basic '..type..'\n', maxNameLength(list)
  
  table.sort(list)
  for _, v in ipairs(list) do
    str = str..'local '..v..string.rep(' ', max - #v)..' = '..v..'\n'
  end
  
  return str
end

local modules, functions = {}, {}
local ID = parser.IDENTIFIER / function (id)
    if basicModules[id] and not modules[id] then
      modules[#modules + 1] = id
      modules[id] = #modules
    elseif basicFunctions[id] and not functions[id] then
      functions[#functions + 1] = id
      functions[id] = #functions
    end
  end

local ALL = P( (ID + 1)^0 ) 

-- TESTING ----------------------------
local args, input = { ... }, nil

if args[1] then
  input = io.open(args[1], 'r')

  if input then -- it's a file
    input = input:read'*a'
  else -- it's a string with code
    input = args[1]
  end
end

subject = input or [=[
local a = _VERSION or math.pi
local table = {} -- false positive

for i, v in ipairs(table.sort(_G[t])) do
  if type(v) == 'function' then
    print(i, v)
  end
end
]=]

ALL:match(subject)

result = buildStatements(modules, 'modules')..'\n'
  ..buildStatements(functions, 'functions')..'\n-- code\n'
  ..subject

--print('subject:', '\n'..subject)
--print('\nresult:', '\n'..result)

print(result)