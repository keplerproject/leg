--[=[
<%
  project.title = "grammar"
  project.description = "LPeg grammar manipulation"
  project.version = "0.2"
  project.date = _G.os.date'%B %d, %Y'
  project.modules = { 'grammar', 'parser' }
%>

# Description

This module defines a handful of operations which can be applied to 
[http://www.inf.puc-rio.br/~roberto/lpeg.html LPeg] patterns and grammars in
general.

# Dependencies

* [http://www.inf.puc-rio.br/~roberto/lpeg.html LPeg].

# Operations

## Piping

Pattern matching dissociates the notion of *matching* from the notion of 
*capturing*: matching checks if a given string follows a certain pattern, 
and capturing generates values according to the match made. This division 
allows interesting possibilities:

* different problems can be solved by applying different captures to the same grammar;
* captures may be defined separately;
* captures may be done on top of other captures.

Accounting for the first and second bullets, the grammar given in 
[parser.html parser] has no captures, enabling the user to reuse it to solve any 
problems that require a Lua grammar. One good example is documentation 
generation, described in a little more detail [#section_Example below].

The third bullet depicts a more interesting idea: a capture might take the 
result of another capture as input, doing a further transformation of the 
original data. This capture chaining, with the latter ones using the former's 
output as its input, is very similar to [http://en.wikipedia.org/wiki/Pipeline_%28Unix%29 Unix pipelines], 
so this mechanism was named **piping**.

## Completing

With piping, several levels of captures can be chained together up to the 
most appropriate for the task at hand. Yet some levels might require extra rules, and modifications to existing ones, to ensure proper matching. 

To avoid manual copying, the new grammar should redefine only the necessary 
rules, copying the rest from the older grammar. This action is dubbed 
**completing**.

## Applying

Once a new rule set is created and [#section_Completing completed], and 
all captures are correctly [#section_Piping piped], all that's left is 
to put them together, a process called **applying**. The result is a grammar ready for [http://www.inf.puc-rio.br/~roberto/lpeg.html#lpeg lpeg.P] 
consumption, whose pattern will return the intended result when a match is made.

## Example

Let's consider the problem of documenting a Lua module. In this case, comments 
must be captured before every function declaration when in the outermost scope:

``
-- -- the code to parse
subject = %[%[
--  -- Calculates the sum a+b. 
--  -- An extra line.
  function sum (a, b)
--  -- code
  end

--  -- f1: assume a variable assignment is not a proper declaration for an 
--  -- exported function
  f1 = function ()
--  -- code
  end

  while true do
--    -- this function is not in the outermost scope
    function aux() end
  end
  
  function something:other(a, ...)
--    -- a global function without comments
  end
%]%]
``

In the code above only `sum` and `something:other` should be documented, as `f1` isn't properly (by our standards) declared and `aux` is not in the outermost scope. 

By combining [http://www.inf.puc-rio.br/~roberto/lpeg.html LPeg] and the modules [parser.html parser] and [grammar.html grammar], this specific problem can be solved as follows:

``
-- -- ye olde imports
local parser, grammar = require 'leg.parser', require 'leg.grammar'
local lpeg = require 'lpeg'

-- -- a little aliasing never hurt anyone
local P, V = lpeg.P, lpeg.V

-- -- change only the initial rule and make no captures
patt = grammar.apply(parser.rules, parser.COMMENT^-1 %* V'GlobalFunction', nil)

-- -- transform the new grammar into a LPeg pattern
patt = P(patt)

-- -- making a pattern that matches any Lua statement, also without captures
Stat = P( grammar.apply(parser.rules, V'Stat', nil) )

-- -- a pattern which matches function declarations and skips statements in
-- -- inner scopes or undesired tokens
patt = (patt + Stat + parser.ANY)^0

-- -- matching a string
patt:match(subject)
``

These are the relevant rules in [parser.html#section_The_Grammar the grammar]:

``
GlobalFunction = 'function' %* FuncName %* FuncBody
FuncName     = ID %* ('.' %* ID)^0 %* (':' %* ID)^-1
FuncBody     = '(' %* (ParList + EPSILON) %* ')' %* Block %* 'end'
ParList      = NameList %* (',' %* '...')^-1
NameList     = ID %* (',' %* ID)^0
ID           = parser.IDENTIFIER
EPSILON      = P(true)
``

It may seem that `ParList + EPSILON` could be substituted for `ParList^-1` (optionally match `ParList`), but then no captures would be made for empty parameter lists, and `GlobalFunction` would get all strings matched by `FuncBody`. The `EPSILON` rule acts in this manner as a placeholder in the argument list, avoiding any argument list processing in the capture function.

Since no captures are being made, [http://www.inf.puc-rio.br/~roberto/lpeg.html#basic lpeg.match] doesn't return anything interesting. Here are some possible captures:

``
-- -- some interesting captures bundled up in a table. Note that the table keys
-- -- match the grammar rules we want to add captures to. Whatever rules aren't in
-- -- the rules table below will come from parser.rules .
captures = {
  %[1%] = function (...) -- the initial rule
    return '&lt;function&gt;'..table.concat{...}..'&lt;/function&gt;' 
  end,
  
  GlobalFunction = function (name, parlist)
    return '&lt;name&gt;'..name..'&lt;/name&gt;&lt;parlist&gt;'..(parlist or '')..'&lt;/parlist&gt;' 
  end,
  
  FuncName = grammar.C, -- capture the raw text
  ParList  = grammar.C, -- capture the raw text
  COMMENT  = parser.comment2text, -- remove the comment trappings
}

-- -- spacing rule
local S = parser.SPACE ^ 0

-- -- rules table
rules = {
  %[1%]     = ((V'COMMENT' %*S) ^ 0) %*S%* V'GlobalFunction',
  COMMENT = parser.COMMENT,
}

-- -- building the new grammar and adding the captures
patt = P( grammar.apply(parser.rules, rules, captures) )

-- -- a pattern that matches a sequence of patts and concatenates the results
patt = (patt + Stat + parser.ANY)^0 / function(...) 
  return table.concat({...}, '\n\n') -- some line breaks for easier reading
end

-- -- finally, matching a string
print(patt:match(subject))
``

`FuncBody` needs no captures, as `Block` and all its non-terminals have none; it 
just needs to pass along any captures made by `ParList`. `NameList` and `ID` also have no captures, and the whole subject string is passed further.

The printed result is:
<pre class="example">
&lt;function&gt;Calculates the sum a+b. An extra line.&lt;name&gt;sum&lt;/name&gt;&lt;parlist&gt;a, b&lt;/parlist&gt;&lt;/function&gt;
<br/>
&lt;function&gt;&lt;name&gt;something:other&lt;/name&gt;&lt;parlist&gt;a, ...&lt;/parlist&gt;&lt;/function&gt;
</pre>
--]=]

-- $Id: grammar.lua,v 1.4 2007/12/07 14:23:56 hanjos Exp $

-- basic modules
local _G    = _G
local table = table

-- basic functions
local assert  = assert
local ipairs  = ipairs
local pairs   = pairs
local pcall   = pcall
local type    = type
local unpack  = unpack

-- imported modules
local lpeg = require 'lpeg'

-- imported functions
local P, V = lpeg.P, lpeg.V

-- module declaration
module 'leg.grammar'

--[[ 
Returns a pattern which matches any of the patterns in `t`.

The iterator `pairs` is used to traverse `t`, so no particular traversal order
is guaranteed. Use [#function_oneOf oneOf] to ensure sequential matching 
attempts.

**Example:**
``
local g, p, m = require 'leg.grammar', require 'leg.parser', require 'lpeg'

-- -- match numbers or operators, capture the numbers
print( (g.anyOf { '+', '-', '%*', '/', m.C(p.NUMBER) }):match '34.5@23 %* 56 / 45 - 45' )
-- --> prints 34.5
``

**Parameters:**
* `t`: a table with LPeg patterns as values. The keys are ignored.

**Returns:**
* a pattern which matches any of the patterns received.
--]]
function anyOf(t)
  local patt = P(false)
  
  for _, v in pairs(t) do
    patt = P(v) + patt
  end
  
  return patt
end

--[[ 
Returns a pattern which matches any of the patterns in `list`.

Differently from [#function_anyOf anyOf], this function ensures sequential 
traversing.

**Parameters:**
* `list`: a list of LPeg patterns.

**Returns:**
* a pattern which matches any of the patterns received.
--]]
function oneOf(list)
  local patt = P(false)
  
  for _, v in ipairs(list) do
    patt = P(v) + patt
  end
  
  return patt
end

--[=[
Returns a pattern which matches a list of `patt`s, separated by `sep`.

**Example:** matching comma-separated values:
``
local g, m = require 'leg.grammar', require 'lpeg'

-- -- separator
local sep = m.P',' + m.P'\n'

-- -- element: anything but sep, capture it
local elem = m.C((1 - sep)^0)

-- -- pattern
local patt = g.listOf(elem, sep)

-- -- matching
print( patt:match %[%[a, b, 'christmas eve'
  d, evening; mate!
  f%]%])
-- --> prints out "a        b       'christmas eve'  d        evening; mate! f"
``

**Parameters:**
* `patt`: a LPeg pattern.
* `sep`: a LPeg pattern.

**Returns:**
* the following pattern: ``patt %* (sep %* patt)^0``
--]=]
function listOf(patt, sep)
  patt, sep = P(patt), P(sep)
  
  return patt * (sep * patt)^0
end

--[[
Returns a pattern which searches for the pattern `patt` anywhere in a string.

This code was extracted from the [http://www.inf.puc-rio.br/~roberto/lpeg.html#ex LPeg home page], in the examples section.

**Parameters:**
* `patt`: a LPeg pattern.

**Returns:**
* a LPeg pattern which searches for `patt` anywhere in the string.
--]]
function anywhere(patt)
  return P { P(patt) + 1 * V(1) }
end

--[[ 
A capture function, made so that `patt / C` is equivalent to `m.C(patt)`. It's intended to be used in capture tables, such as those required by [#function_pipe pipe] and [#function_apply apply].
--]]
function C(...) return ... end

--[[ 
A capture function, made so that `patt / Ct` is equivalent to `m.Ct(patt)`. It's intended to be used in capture tables, such as those required by [#function_pipe pipe] and [#function_apply apply].
--]]
function Ct(...) return { ... } end

--[[
Creates a shallow copy of `grammar`.

**Parameters:**
* `grammar`: a regular table.

**Returns:**
* a newly created table, with `grammar`'s keys and values.
--]]
function copy(grammar)
	local newt = {}
  
	for k, v in pairs(grammar) do
		newt[k] = v
	end
  
	return newt
end

--[[
[#section_Completing Completes] `dest` with `orig`.

**Parameters:**
* `dest`: the new grammar. Must be a table.
* `orig`: the original grammar. Must be a table.

**Returns:**
* `dest`, with new rules inherited from `orig`.
--]]
function complete (dest, orig)
	for rule, patt in pairs(orig) do
		if not dest[rule] then
			dest[rule] = patt
		end
	end
  
	return dest
end

--[[
[#section_Piping Pipes] the captures in `orig` to the ones in `dest`.

`dest` and `orig` should be tables, with each key storing a capture function. Each capture in `dest` will be altered to use the results for the matching one in `orig` as input, using function composition. Should `orig` possess keys not in `dest`, `dest` will copy them.

**Parameters:**
* `dest`: a capture table.
* `orig`: a capture table.

**Returns:**
* `dest`, suitably modified.
--]]
function pipe (dest, orig)
	for k, vorig in pairs(orig) do
		local vdest = dest[k]
		if vdest then
			dest[k] = function(...) return vdest(vorig(...)) end
		else
			dest[k] = vorig
		end
	end
	
	return dest
end

--[[
[#section_Completing Completes] `rules` with `grammar` and then [#Applying applies] `captures`.     

`rules` can either be:
* a single pattern, which is taken to be the new initial rule, 
* a possibly incomplete LPeg grammar table, as per [#function_complete complete], or 
* `nil`, which means no new rules are added.

`captures` can either be:
* a capture table, as per [#function_pipe pipe], or
* `nil`, which means no captures are applied.

**Parameters:**
* `grammar`: the old grammar. It stays unmodified.
* `rules`: optional, the new rules. 
* `captures`: optional, the final capture table.

**Returns:**
* `rules`, suitably augmented by `grammar` and `captures`.
--]]
function apply (grammar, rules, captures)
  if rules == nil then
    rules = {}
  elseif type(rules) ~= 'table' then
    rules = { rules }
  end
  
  complete(rules, grammar)
  
  if type(grammar[1]) == 'string' then
    rules[1] = V(grammar[1])
  end
	
	if captures ~= nil then
		assert(type(captures) == 'table', 'captures must be a table')
    
		for rule, cap in pairs(captures) do
			rules[rule] = rules[rule] / cap
		end
	end
  
	return rules
end

--[[
Returns a pattern which simply fails to match if an error is thrown during the matching.

One usage example is [parser.html#variable_NUMBER parser.NUMBER]. Originally it threw an error when trying to match a malformed number (such as 1e23e4), since in this case the input is obviously invalid and the pattern would be part of the Lua grammar. So [#function_pmatch pmatch] is used to catch the error and return `nil` (signalling a non-match) and the error message.

**Parameters:**
* `patt`: a LPeg pattern.

**Returns:**
* a pattern which catches any errors thrown during the matching and simply doesn't match instead of propagating the error.
--]]
function pmatch(patt)
  patt = P(patt)
  return P(function (subject, i)
    local results = { pcall(patt.match, patt, subject, i) }
    local status = table.remove(results, 1)
    
    if status then
      return unpack(results)
    else
      return nil, unpack(results)
    end
  end)
end
