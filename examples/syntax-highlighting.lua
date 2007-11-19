-------------------------------------------------------------------------------
-- 
-- A syntax highlighter. Highlights some lexical patterns. Do 
-- lua syntax-highlighting.lua > syntax.html to see the result in a browser.
--
-- Author: Humberto Anjos
-- 
-- $Id: syntax-highlighting.lua,v 1.2 2007/11/19 13:34:47 hanjos Exp $
--
-------------------------------------------------------------------------------

-- import the fellas
local lpeg = require 'lpeg'

local leg     = require 'leg'
local scanner = leg.scanner
local parser  = leg.parser

-- aliasing...
local Cs, V, P = lpeg.Cs, lpeg.V, lpeg.P

-- arguments
local args = { ... }

-- the code to parse
subject = args[1] or [==[
  local a, b = x'BOOM, baby!' + 2
  
  function hit_it(t) -- and quit it
    local points = 0
    
    for i, v in ipairs(t) do
      print "Miller time!"
      
      points = points + (math.random() < 0.5) and 3 or 2
    end
    
    return points
  end
  
  --[[ I need a multi-line comment for good measure. Hum...
  
    I want my baby back, baby back, baby back...
    I want my baby back, baby back, baby back...
    
    CHILI! Baby back ribs! I want my baby back ribs...
  ]]
  _G.print [=[
    Attempting to print out a long, multi-level string to 
      [[ demonstrate the ineffable, zen-like beauty of this code. ]]
    So many diferent ways to do this, but all is one in the Tao.
    
    Wait a minute, what the hell am I talking about?
  ]=]
]==]

-- now the magic happens....

-- the colors...
local commentColor  = '#808080' -- gray
local stringColor   = '#008000' -- dark green
local numberColor   = '#B00000' -- red
local keywordColor  = '#0000FF' -- blue

-- the patterns...
local COMMENT = scanner.COMMENT / function (c) 
  return '<font color="'..commentColor..'"> '..c..'</font>' 
end

local STRING = scanner.STRING / function (c) 
  return '<font color="'..stringColor..'">'..c..'</font>' 
end

local NUMBER = scanner.NUMBER / function (c)
  return '<i><font color="'..numberColor..'">'..c..'</font></i>' 
end

local KEYWORD = scanner.KEYWORD / function (c) 
  return '<b><font color="'..keywordColor..'">'..c..'</font></b>' 
end

-- opening tags
local BOF = scanner.BOF / function () return '<html><body><pre>' end

-- closing tags
local EOF = scanner.EOF / function () return '</pre></body></html>' end

-- this is here just to keep identifiers like bin2c from being parsed by NUMBER
local ID = scanner.IDENTIFIER

-- the substitution pattern. BOF and EOF are there to ensure that the
-- opening and closing tags are there to make the result a valid HTML page.
local patt = Cs( BOF* (COMMENT + STRING + ID + NUMBER + KEYWORD + 1)^0 *EOF )

-- voilà!
print(patt:match(subject))
