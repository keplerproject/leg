-------------------------------------------------------------------------------
-- 
-- A preprocessor which uses Leg to transform ...[<exp>] into 
-- select(<exp>, ...).
--
-- Author: Humberto Anjos
--
-- $Id: vararg-indexing.lua,v 1.1 2007/11/12 20:32:01 hanjos Exp $
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
  -- Oh my G0dZ, a comment in the middle !!1!one!1!
  [-(-3)]
]=]

-- spacing rule
local S = parser.rules.IGNORED -- scanner.IGNORED or V'IGNORED' could be used here

-- matching ...[<exp>], and making the capture return the new value.
-- We'll use lpeg.Cs for the substitution.
local Index = V'...' *S* V'[' *S* lpeg.C(V'Exp') *S* V']' / function (exp) 
  return 'select('..exp..', ...)'
end

-- a pattern which matches any instance of Index and returns 'select(<Exp>, ...)'
-- You need parser.apply because the definition of Exp is recursive, and needs
-- roughly half of Lua's grammar to work. One could try to work out which rules
-- are actually needed, but just using the whole damn thing is so much easier... 
local VARARG = P( parser.apply(Index) )

-- a pattern which searches the entire subject for VARARG, and then uses Cs to do
-- the substitution
local ALL = Cs( (VARARG + 1)^0 )

-- printing the results
print('subject:', '\n'..subject)
print('result:', '\n'..ALL:match(subject))
