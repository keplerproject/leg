-------------------------------------------------------------------------------
-- 
-- A preprocessor which uses Leg to transform ...[<exp>] into 
-- select(<exp>, ...).
--
-- Author: Humberto Anjos
--
-- $Id: vararg-indexing.lua,v 1.2 2007/11/19 13:34:47 hanjos Exp $
-- 
-------------------------------------------------------------------------------

-- ye olde imports
local lpeg = require 'lpeg'

local parser = require 'leg.parser'

-- some aliasing to save my poor fingertips
local V, P, Cs = lpeg.V, lpeg.P, lpeg.Cs

-- argument processing
local args = { ... }

-- the code to parse
subject = args[1] or [=[
  local arg1, arg2, arg3 = ...[1], ... [ -2 +x[[whatever, man]]^t[5]  ], ... 
  -- Oh my G0dZ, a comment in the middle !!1!one!1! This will disappear
  [-(-3)]
  
  if do_or_die() then -- inside a block
    return ...[BOOM_baby(...[2], 'boink!')] -- inside an expression
  end
  
  -- ...['inside a comment']
  
  a = " ...['inside a string!'] "
]=]

-- spacing rule
local S = parser.rules.IGNORED -- scanner.IGNORED or V'IGNORED' could be used

-- a pattern which matches any instance of ...[<exp>] and returns 
-- 'select(<Exp>, ...)'. You need parser.apply because the definition of Exp is
-- recursive, and needs roughly half of Lua's grammar to work. One could try to
-- work out which rules are actually needed, but just using the whole damn 
-- thing is so much easier... 
local oldExp = parser.rules.Exp

local VARARG = P( parser.apply (
  { -- the rule table
    
    -- matching ...[<exp>]. We'll use lpeg.Cs for the substitution. 
    VarargIndex = V'...' *S* V'[' *S* V'Exp' *S* V']',
    
    -- VarargIndex is now a valid subexpression. Using lpeg.Cs ensures that 
    -- inner VarargIndexes will be substituted as well. VarargIndex must be 
    -- matched before oldExp or the ... will be understood as a normal 
    -- ellipsis.
    Exp = Cs(V'VarargIndex' + oldExp),
  }, 
  { -- the capture table
    VarargIndex = function (exp) 
      return 'select('..exp..', ...)'
    end
  }) )

-- a pattern which does the substitution with Cs 
local ALL = Cs(VARARG)

-- printing the results
print('subject:', '\n'..subject)
print('result:', '\n'..ALL:match(subject))
