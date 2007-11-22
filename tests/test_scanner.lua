-------------------------------------------------------------------------------
-- Scanner test suite for Leg
--
-- Authors: Humberto Anjos
-- Copyright (c) 2007 Leg
--
-- $Id: test_scanner.lua,v 1.2 2007/11/22 21:15:24 hanjos Exp $
--
-------------------------------------------------------------------------------

local scanner = require 'leg.scanner'

print 'Testing keyword matching...'

for KEY, patt in pairs(scanner.keywords) do
  local key = string.lower(KEY)
  
  -- The key alone
  assert(patt:match(key))
  
  -- The key in uppercase
  assert(not patt:match(KEY))
  
  -- The key twice
  assert(not patt:match(key..key))
  
  -- The key at the end of something
  assert(not patt:match('yadda_'..key))
  
  -- The key at the beginning of something
  assert(not patt:match(key..'yaddayadda'))
end

print '...OK!'
print 'Testing symbol matching...'

for symbol, patt in pairs(scanner.symbols) do
  -- The symbol alone
  assert(patt:match(symbol))
  
  -- The symbol at the end of something
  assert(not patt:match('yadda '..symbol))
  
  -- The symbol at the beginning of something
  assert(patt:match(symbol..' yadda') == #symbol + 1)
end

print '...OK!'
print 'Testing IDENTIFIER...'

local ID = scanner.IDENTIFIER

-- a keyword
assert(not ID:match'and')

-- a keyword in the name
assert(ID:match'andale' == 7)

-- an operator
assert(ID:match'make-name' == 5)

-- spaces
assert(ID:match'one two three' == 4)

-- invalid characters
assert(not ID:match'!!name$')

-- accent marks
assert(ID:match'aãoéç' == 6)

-- weird, but should match
assert(ID:match'_123' == 5)

-- starting with a digit
assert(not ID:match'1abc')

-- digits at the end
assert(ID:match'abc1234' == 8)

-- all underscores
assert(ID:match'___' == 4)

-- weird characters
assert(ID:match'\127\128\129ç' == 5)

print '...OK!'
print 'Testing NUMBER...'

local NUM = scanner.NUMBER

-- natural number
assert(NUM:match'123')

-- natural number
assert(NUM:match'101')

-- natural number
assert(NUM:match'001234567890')

-- decimal point
assert(NUM:match'.123')

-- decimal point
assert(NUM:match'1.')

-- decimal point
assert(NUM:match'12.343')

-- hexadecimal notation
assert(NUM:match'0x7a2fF')

-- hexadecimal notation (yes, this matches)
assert(NUM:match'0x')

-- hexadecimal notation
assert(not NUM:match'!ff')

-- scientific notation
assert(NUM:match'1e-23')

-- scientific notation
assert(NUM:match'9.045e+3')

-- scientific notation
assert(NUM:match'.00678e2')

-- scientific notation (works, but blows up in flames)
--assert(not NUM:match'1.23e4e5')

-- negative number (this is the unary operator - applied to 0.1)
assert(not NUM:match'-.1')

-- malformed number (works, but blows up in flames)
--assert(NUM:match'123and')

print '...OK!'
print 'Testing STRING...'

local STR = scanner.STRING

-- single quotes
assert(STR:match"'What a pity...'")

-- single quotes with escape characters
assert(STR:match"'What \065 \112\105\116\121...'")

-- unclosed single quotes (works, but blows up in flames)
--assert(not STR:match"'what?")

-- single quotes with line breaks inside (works, but blows up in flames)
--assert(not STR:match"'what\n?'")

-- single quotes with valid line breaks inside
assert(STR:match"'what\\\n?'")

-- single quotes, escaped
assert(STR:match"'What a \\'pity\\'...'")

-- single quotes, with double quotes inside
assert(STR:match"'What a \"pity\"...'")

-- double quotes
assert(STR:match'"What a pity..."')

-- double quotes with escape characters
assert(STR:match'"What \065 \112\105\116\121..."')

-- unclosed double quotes (works, but blows up in flames)
--assert(not STR:match'"what?')

-- double quotes with line breaks inside (works, but blows up in flames)
--assert(not STR:match'"what\n?"')

-- double quotes with valid line breaks inside
assert(STR:match'"what\\\n?"')

-- double quotes, escaped
assert(STR:match'"What a \\"pity\\"..."')

-- double quotes, with single quotes inside
assert(STR:match'"What a \'pity\'..."')

-- long strings
assert(STR:match"[[If not for you...]]")

-- multi-line long strings
assert(STR:match"[[something\n\or\nanother]]")

-- embedded long strings
assert(STR:match"[=[fantasy for sale [[that's entertainment!]]]=]")

-- long strings with quotes inside
assert(STR:match"[[Package it like a \"rebel\" or a 'hero']]")

-- unclosed long strings (works, but blows up in flames)
--assert(not STR:match"[=[[[Reality]] withdraws")

print '...OK!'
print 'Testing COMMENT...'

local COM = scanner.COMMENT

-- single line comment
assert(COM:match'-- single line comment\nsome code here' == 23)

-- single line comment, no line break
assert(COM:match'-- she can manipulate reactions')

-- multi-line comment
assert(COM:match'--[[superconductor!\nThat\'s entertainment!]]')

-- multi-line comment, alternate closing
assert(COM:match'--[[superconductor!\nThat\'s entertainment!\n--]]')

-- unclosed comment (works, but blows up in flames)
--assert(not COM:match'--[[<waiting for The Clansman to start>')

-- embedded multi-line comment
assert(COM:match'--[=[That belongs to --[[the clan]]]=]')

-- embedded multi-line comment
assert(COM:match[===[--[[ multi-line
--[==[
asdaks \n
-- --[[ deve pegar comment multi-line
--print'oi' ]] ]===])

print '...OK!'