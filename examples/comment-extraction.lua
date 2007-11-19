-------------------------------------------------------------------------------
-- 
-- The comment extraction example given in doc/grammar.html. For those of you 
-- who haven't read it, the code and comments are available below. 
--
-- Authors: Humberto Anjos and Francisco Sant'Anna
-- 
-- $Id: comment-extraction.lua,v 1.2 2007/11/19 13:34:47 hanjos Exp $
--
-------------------------------------------------------------------------------

-- some imports to get things started.
local lpeg = require 'lpeg'

local parser  = require 'leg.parser'
local scanner = require 'leg.scanner'
local grammar = require 'leg.grammar'

-- some aliasing
local P, V = lpeg.P, lpeg.V

-- argument capturing
local args = { ... }

--
-- Let's consider the problem of documenting a Lua module. In this case, comments 
-- must be captured before every function declaration when in the outermost scope.
--

-- the code to parse
subject = args[1] or [=[
  -- Calculates the sum a+b. 
  -- An extra line.
  function sum (a, b)
  -- code
  end

  -- a variable assignment is not a "proper" declaration for an 
  -- exported function
  f1 = function ()
  -- code
  end

  while true do
    -- this function is not in the outermost scope
    function aux() end
  end
  
  function something:other(a, ...)
    -- a global function without comments
    
    return a, ... -- won't appear in the result
  end
]=]

-- In the code above we want only to document sum and something:other, as 
-- f1 isn't properly (by our standards) declared and aux is not in the 
-- outermost scope (although it is still a global function).
--
-- Let's define some patterns to simplify our job:

-- spacing rule
local S = scanner.SPACE ^ 0

-- matches any Lua statement, no captures
Stat = P( parser.apply(V'Stat', nil) )

-- some interesting captures bundled up in a table. Note that the table keys
-- match the grammar rules we want to add captures to. Any rules not in the
-- `rules` table below will come from parser.rules .
captures = {
  [1] = function (...) -- the initial rule
    return '<function>'..table.concat{...}..'</function>' 
  end,
  
  GlobalFunction  = function (name, parlist) -- global function declaration
    return '<name>'..name..'</name><parlist>'..(parlist or '')..'</parlist>' 
  end,
  
  FuncName  = grammar.C, -- capture the raw text
  ParList   = grammar.C, -- capture the raw text
  COMMENT   = scanner.comment2text, -- extract comment trappings
}

-- the rules table
rules = {
  [1] = ((V'COMMENT' *S) ^ 0) *S* V'GlobalFunction', -- new initial rule
  COMMENT = scanner.COMMENT, -- just to add COMMENT's capture to the capture table
}

-- building the new grammar and adding the captures. This pattern will match
-- any global function optionally preceded by comments, and return a string
-- in the following format: 
-- 
-- <function>comments<name>name</name><parlist>parameter list</parlist></function>
commentedFunc = P( grammar.apply(parser.rules, rules, captures) ) 

-- finally, this pattern matches all commented global functions, Stats 
-- or any other Lua tokens and packages the results in a table. This is done
-- to capture only global function declarations in the global scope.
patt = (commentedFunc + Stat + scanner.ANY)^0 / function(...) 
  return table.concat({...}, '\n\n') -- some line breaks for easier reading
end

-- now match subject
print('subject:', '\n'..subject)
print('result:', '\n'..patt:match(subject))
