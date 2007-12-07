--[[
<%
  project.title = "parser"
  project.description = "Lua 5.1 parser"
  project.version = "0.2"
  project.date = _G.os.date'%B %d, %Y'
  project.modules = { 'grammar', 'parser' }
%>

# Description

Exports Lua 5.1's lexical patterns and syntactic rules.

# Dependencies

* [http://www.inf.puc-rio.br/~roberto/lpeg.html LPeg]; and
* [grammar.html grammar].

# The Grammar

The [#variable_rules rules] variable implements the official [http://www.lua.org/manual/5.1/manual.html#8 Lua 5.1 grammar]. It includes all lexical rules, as well as the `CHUNK` rule, which matches a complete Lua source file.

[#variable_rules rules] is a table with [http://www.inf.puc-rio.br/~roberto/lpeg.html#grammar open references], not yet a LPeg pattern; to create a pattern, it must be given to `[http://www.inf.puc-rio.br/~roberto/lpeg.html#lpeg lpeg.P]`. This is done to enable users to modify the grammar to suit their particular needs. [grammar.html grammar] provides a small API for this purpose.

The code below shows the Lua 5.1 grammar in LPeg, minus spacing issues.

The following convention is used for rule names:
* **TOKENRULE**: token rules (which represent terminals) are in upper case when applicable (ex. `+, WHILE, NIL, ..., THEN, {, ==`).
* **GrammarRule**: the main grammar rules (non-terminals): Examples are `Chunk`, `FuncName`, `BinOp`, and `TableConstructor`.
* **_GrammarRule**: subdivisions of the main rules, introduced to ease captures. Examples are `_SimpleExp`, `_PrefixExpParens` and `_FieldExp`.
* **METARULE**: grammar rules with a special semantic meaning, to be used for capturing in later modules, like `BOF`, `EOF` and `EPSILON`.

``
rules = {
--   -- See peculiarities below
    EPSILON = lpeg.P(true)
    EOF     = EOF -- end of file rule
    BOF     = BOF -- beginning of file rule
    Name    = ID

--   -- Default initial rule
    %[1%]     = CHUNK
    CHUNK   = BANG^-1 %* Block

    Chunk   = (Stat %* ';'^-1)^0 %* (LastStat %* ';'^-1)^-1
    Block   = Chunk

--   -- STATEMENTS
    Stat          = Assign + FunctionCall + Do + While + Repeat + If
                  + NumericFor + GenericFor + GlobalFunction + LocalFunction
                  + LocalAssign
    Assign        = VarList %* '=' %* ExpList
    Do            = 'do' %* Block %* 'end'
    While         = 'while' %* Exp %* 'do' %* Block %* 'end'
    Repeat        = 'repeat' %* Block %* 'until' %* Exp
    If            = 'if' %* Exp %* 'then' %* Block
                      %* ('elseif' %* Exp %* 'then' %* Block)^0
                      %* (('else' %* Block) + EPSILON)
                      %* 'end'
    NumericFor    = 'for' %* Name %* '='
                      %* Exp %* ',' %* Exp %* ((',' %* Exp) + EPSILON)
                      %* 'do' %* Block %* 'end'
    GenericFor    = 'for' %* NameList %* 'in' %* ExpList %* 'do' %* Block %* 'end'
    GlobalFunction = 'function' %* FuncName %* FuncBody
    LocalFunction = 'local' %* 'function' %* Name %* FuncBody
    LocalAssign   = 'local' %* NameList %* ('=' %* ExpList)^-1
    LastStat      = 'return' %* ExpList^-1
                  + 'break'

--   -- LISTS
    VarList  = Var %* (',' %* Var)^0
    NameList = Name %* (',' %* Name)^0
    ExpList  = Exp %* (',' %* Exp)^0

--   -- EXPRESSIONS
    Exp          = _SimpleExp %* (BinOp %* _SimpleExp)^0
    _SimpleExp   = 'nil' + 'false' + 'true' + Number + String + '...' + Function
                 + _PrefixExp + TableConstructor + (UnOp %* _SimpleExp)
    _PrefixExp   = ( Name                  a Var
                   + _PrefixExpParens      only an expression
                   ) %* (
                       _PrefixExpSquare    a Var
                     + _PrefixExpDot       a Var
                     + _PrefixExpArgs      a FunctionCall
                     + _PrefixExpColon     a FunctionCall
                   ) ^ 0

--   -- Extra rules for semantic actions:
    _PrefixExpParens = '(' %* Exp %* ')'
    _PrefixExpSquare = '[' %* Exp %* ']'
    _PrefixExpDot    = '.' %* ID
    _PrefixExpArgs   = Args
    _PrefixExpColon  = ':' %* ID %* _PrefixExpArgs

--   -- These rules use an internal trick to be distingished from _PrefixExp
    Var              = _PrefixExp
    FunctionCall     = _PrefixExp

--   -- FUNCTIONS
    Function     = 'function' %* FuncBody
    FuncBody     = '(' %* (ParList+EPSILON) %* ')' %* Block %* 'end'
    FuncName     = Name %* _PrefixExpDot^0 %* ((':' %* ID)+EPSILON)
    Args         = '(' %* (ExpList+EPSILON) %* ')'
                 + TableConstructor + String
    ParList      = NameList %* (',' %* '...')^-1
                 + '...'

--   -- TABLES
    TableConstructor = '{' %* (FieldList+EPSILON) %* '}'
    FieldList        = Field %* (FieldSep %* Field)^0 %* FieldSep^-1
    FieldSep         = ',' + ';'

--   -- Extra rules for semantic actions:
    _FieldSquare     = '[' %* Exp %* ']' %* '=' %* Exp
    _FieldID         = ID %* '=' %* Exp
    _FieldExp        = Exp

--   -- OPERATORS
    BinOp    = '+' + '-' + '%*' + '/' + '^' + '%' + '..'
             + '&lt;' + '&lt;=' + '&gt;' + '&gt;=' + '==' + '~='
             + 'and' + 'or'
    UnOp     = '-' + 'not' + '#'

--   -- IDENTIFIERS
  , ID      = ([_A-Za-z] * ([0-9_A-Za-z])^0) - Keyword
  , Keyword = ... -- any of Lua's keywords
  , Symbol  = ... -- any of Lua's symbols
  
--   -- ...plus a rule for each keyword and symbol
}
``

The implementation has certain peculiarities that merit clarification:

* Spacing is matched only between two tokens in a rule, never at the beginning or the end of a rule.
 
* `EPSILON` matches the empty string, which means that it always succeeds without consuming input. Although `rule + EPSILON` can be changed to `rule^-1` without any loss of syntactic power, `EPSILON` was introduced in the parser due to it's usefulness as a placeholder for captures.

* `BOF` and `EOF` are rules used to mark the bounds of a parsing match, and are useful for semantic actions.

* `Name` versus `ID`: the official Lua grammar doesn't distinguish between them, as their syntax is exactly the same (Lua identifiers). But semantically `Name` is a variable identifier, and `ID` is used with different meanings in `_FieldID`, `FuncName`, `_PrefixExpColon` and `_PrefixExpDot`.

* In Lua's [http://www.lua.org/manual/5.1/manual.html#8 original extended BNF grammar], `Var` and `FunctionCall` are defined using left recursion, which is unavailable in PEGs. In this implementation, the problem was solved by modifying the PEG rules to eliminate the left recursion, and by setting some markers (with some LPeg chicanery) to ensure the proper pattern is being used.
--]]

-- $Id: parser.lua,v 1.4 2007/12/07 14:23:56 hanjos Exp $

-- basic modules
local _G     = _G
local table  = table
local string = string

-- basic functions
local assert    = assert
local error     = error
local pairs     = pairs
local pcall     = pcall
local print     = print
local require   = require
local tonumber  = tonumber
local tostring  = tostring

-- imported modules
local m       = require 'lpeg'

local grammar = require 'leg.grammar'

-- imported functions
local V, P, S, R, Cs = m.V, m.P, m.S, m.R, m.Cs

-- module declaration
module 'leg.parser'

----------------------------- HELPER FUNCTIONS --------------------------------

-- Searches for the last substring in s which matches pattern
local function rfind(s, pattern, init, finish)
  init = init or #s
  finish = finish or 1
  
  for i = init, finish, -1 do
    local lfind, rfind = string.find(s, pattern, i)
    
    if lfind and rfind then
      return lfind, rfind
    end
  end
  
  return nil
end

-- Counts the number of lines (separated by *'\n'*) in `subject`.
local function lines (subject)
	local inc = function (l) return l + 1 end
	local L = m.Ca( m.Cc(1) * (P'\n'/inc + P(1)) ^0 )

	return L:match(subject)
end

-- Takes a list of strings, and returns a table with the original values 
-- (in uppercase) as the keys and the respective patterns as the values.
local function topatterns(t, minus)
  local newt = {}
  local upper = string.upper
  
  for _, v in pairs(t) do
    local patt = P(v)
    
    if minus then
      patt = patt * minus
    end
    
    newt[upper(v)] = patt
  end
  
  return newt
end

-- throws an error if the grammar rule `rule` doesn't match
-- `desc` is there for a slightly better error message
local function CHECK(rule, desc)
  patt, desc = V(rule), desc or 'chunk'
  
  return patt + P(function (s, i)
    local line = lines(s:sub(1, i))
    local vicinity = s:sub(i-5, i+5):gsub("\n", "<EOL>")
    
    _G.error('Malformed '..desc..' in line '..line..', near "'..vicinity..'": a "'..rule:lower()..'" is missing!', 0)
  end)

end

--[[
Returns a function which throws lexical errors.

**Parameters:**
* `msg`: the message to be concatenated to the error message.

**Returns:**
* A function built to be used as a [http://www.inf.puc-rio.br/~roberto/lpeg.html#lpeg LPeg pattern], which will throw an error when matched.

Usage example:
``
patt = intended_pattern^0 %* (EOF + error'character after EOF')
``

It may also be used as a normal function:
``
function (subject, i)
  if bad_condition then
    error'bad condition'(subject, i)
  end
end
``
--]]
local function lexicalError (msg)
	return function (subject, i)
		local line = lines(string.sub(subject,1,i))
    
    _G.error('Lexical error in line '..line..', near "'
      ..(subject:sub(i-10,i)):gsub('\n','EOL')..'": '..msg, 0)
	end
end

----------------------------- HELPER PATTERNS ---------------------------------

-- digit pattern
local N = R'09'

-- alphanumeric pattern
local AZ = R('__','az','AZ','\127\255')     -- llex.c uses isalpha()

----------------------------- EXPORTED PATTERNS -------------------------------

-- tries to implement the same read_numeral() as in llex.c
local number = function (subject, i)
	--               [.\d]+     .. ( [eE]  ..   [+-]? )?    .. isalnum()*
	local patt = (P'.' + N)^1 * (S'eE' * S'+-'^-1)^-1 * (N+AZ)^0
	patt = patt / function(num)
    if not tonumber(num) then 
      lexicalError('Malformed number: '..tostring(num))(subject,i) 
      --return nil -- don't match
    end 
  end
  
	return m.match(patt, subject, i)
end

-- Matches any Lua number.
-- <% exp = 'LPeg pattern' %>
NUMBER = #(N + (P'.' * N)) * grammar.pmatch(number)

-- LONG BRACKETS
local long_brackets = #(P'[' * P'='^0 * P'[') * function (subject, i1)
	local level = assert( subject:match('^%[(=*)%[', i1) )
	local _, i2 = subject:find(']'..level..']', i1, true)  -- true = plain "find substring"
	return (i2 and (i2+1)) or lexicalError('unfinished long brackets')(subject, i1)
end

-- Matches any Lua string.
-- <% exp = 'LPeg pattern' %>
STRING = nil
do
	local Str1 = P'"' * ( (P'\\' * 1) + (1 - (S'"\n\r\f')) )^0 * (P'"' 
    + lexicalError'unfinished string')
    
	local Str2 = P"'" * ( (P'\\' * 1) + (1 - (S"'\n\r\f")) )^0 * (P"'" 
    + lexicalError'unfinished string')
    
	local Str3 = long_brackets
  
	STRING = grammar.pmatch(Str1 + Str2 + Str3)
end

local multi  = P'--' * long_brackets
local single = P'--' * (1 - P'\n')^0

-- Matches any Lua comment.
-- <% exp = 'LPeg pattern' %>
COMMENT = grammar.pmatch(multi + single) 
-- multi must be the first ( --[ is a one line comment )

-- Matches the beginning of a file.
-- <% exp = 'LPeg pattern' %>
BOF = P(function(s,i) return (i==1) and i end)

-- Matches the end of a file.
-- <% exp = 'LPeg pattern' %>
EOF = P(-1)

-- Matches UNIX's shebang (e.g. `#!/usr/bin/lua`).
-- <% exp = 'LPeg pattern' %>
BANG = BOF * P'#!' * ( P(1) - '\n' )^0 * '\n'

-- Matches any space character.
-- <% exp = 'LPeg pattern' %>
SPACE = S'\n \t\r\f'

-- Matches everything ignored by the parser.
-- <% exp = 'LPeg pattern' %>
IGNORED = (SPACE + COMMENT)^0

----------------------------- GRAMMAR -----------------------------------------

-- a little LPeg trick to enable a limited form of left recursion
local prefix 

-- sets a prefix used to distinguish between Var and FunctionCall
local setPrefix = function (p)
	return function (_,i)
		prefix = p
		return i
	end
end

-- matches Var and FunctionCall from _PrefixExp
local matchPrefix = function (p)
	return function (_, i)
		return (prefix == p) and i
	end
end

-- A table of Lua symbols, indexed by the matching string.
local symbols = topatterns {
	'+',     '-',     '*',     '/',     '%',     '^',     '#',
	'==',    '~=',    '<=',    '>=',    '<',     '>',     '=',
	'(',     ')',     '{',     '}',     '[',     ']',
	';',     ':',     ',',     '.',     '..',    '...',
}

-- special cases (they need lookahead)
symbols['-']  = symbols['-']  - '--'
symbols['<']  = symbols['<']  - symbols['<=']
symbols['>']  = symbols['>']  - symbols['>=']
symbols['=']  = symbols['=']  - symbols['==']
symbols['[']  = symbols['[']  - '[' * S'[='
symbols['.']  = symbols['.']  - (symbols['..'] + N)
symbols['..'] = symbols['..'] - symbols['...']

-- A table of Lua keywords, indexed by the matching uppercase string.
local keywords = topatterns ({
    'and',      'break',    'do',       'else',     'elseif',
    'end',      'false',    'for',      'function', 'if',
    'in',       'local',    'nil',      'not',      'or',
    'repeat',   'return',   'then',     'true',     'until',    'while',
  }, -(N + AZ))

-- this will be used a lot below
local _, listOf, anyOf = 
  V'IGNORED', grammar.listOf, grammar.anyOf

--[[ 
A table holding the Lua 5.1 grammar. See [#section_The_Grammar The Grammar] for an extended explanation.
--]]
-- <% exp = 'table' %>
rules = {
	  IGNORED = IGNORED  -- seen as _ below
	, EPSILON = P(true)
  , EOF     = EOF
  , BOF     = BOF
  , NUMBER  = NUMBER
	, STRING  = STRING
  , COMMENT = COMMENT
	, Name    = V'ID'
  
  -- CHUNKS
	, [1]     = V'CHUNK'
	, CHUNK   = BANG^-1 * V'Block'

	, Chunk   = (_* V'Stat' *_* V';'^-1)^0 
            *_* (V'LastStat' *_* V';'^-1)^-1
	, Block   = V'Chunk'

	-- STATEMENTS
	, Stat        = V'Assign' + V'FunctionCall' + V'Do' 
                + V'While' + V'Repeat' + V'If'
                + V'NumericFor' + V'GenericFor' 
                + V'GlobalFunction' + V'LocalFunction' 
                + V'LocalAssign'
	, Assign      = V'VarList' *_* V'=' *_* V'ExpList'
	, Do          = V'DO' *_* V'Block' *_* CHECK('END', 'do block')
	, While       = V'WHILE' *_* V'Exp' *_* CHECK('DO', 'while loop')
                  *_* V'Block' *_* CHECK('END', 'while loop')
	, Repeat      = V'REPEAT' *_* V'Block' 
                  *_* CHECK('UNTIL', 'repeat loop') *_* V'Exp'
	, If          = V'IF' *_* V'Exp' *_* CHECK('THEN', 'then block') 
                *_* V'Block' * (_* V'ELSEIF' *_* V'Exp' 
                *_* CHECK('THEN', 'elseif block') *_* V'Block')^0
                * ((_* V'ELSE' * V'Block') + V'EPSILON')
                * _* CHECK('END', 'if statement')
  , NumericFor  = V'FOR' *_* V'Name' *_* V'=' *_* V'Exp' 
                *_* V',' *_* V'Exp' 
                *_* ((V',' *_* V'Exp') + V'EPSILON')
                *_* CHECK('DO', 'numeric for loop') *_* V'Block' 
                *_* CHECK('END', 'numeric for loop')
	, GenericFor    = V'FOR' *_* V'NameList' *_* V'IN' 
                  *_* V'ExpList' *_* CHECK('DO', 'generic for loop') 
                  *_* V'Block' *_* CHECK('END', 'generic for loop')
	, GlobalFunction = V'FUNCTION' *_* V'FuncName' *_* V'FuncBody'
	, LocalFunction = V'LOCAL' *_* V'FUNCTION' *_* V'Name' 
                  *_* V'FuncBody'
	, LocalAssign   = V'LOCAL' *_* V'NameList' 
                  * (_* V'=' *_* V'ExpList')^-1
	, LastStat      = V'RETURN' * (_* V'ExpList')^-1
                  + V'BREAK'

	-- LISTS
	, VarList   = listOf(V'Var' , _* V',' *_)
  , NameList  = listOf(V'Name', _* V',' *_)
  , ExpList   = listOf(V'Exp' , _* V',' *_)

	-- EXPRESSIONS
	, Exp             = V'_SimpleExp' * (_* V'BinOp' *_* V'_SimpleExp')^0
	, _SimpleExp      = V'NIL' + V'FALSE' + V'TRUE' + V'NUMBER' 
                    + V'STRING' + V'...' + V'Function' + V'_PrefixExp' 
                    + V'TableConstructor' + (V'UnOp' *_* V'_SimpleExp')
	, _PrefixExp      = ( V'Name'             * setPrefix'Var'  -- Var
                    + V'_PrefixExpParens'   * setPrefix(nil)) -- removes last prefix
                    * (_* (
                        V'_PrefixExpSquare' * setPrefix'Var'  -- Var
                      + V'_PrefixExpDot'    * setPrefix'Var'  -- Var
                      + V'_PrefixExpArgs'   * setPrefix'Call' -- FunctionCall
                      + V'_PrefixExpColon'  * setPrefix'Call' -- FunctionCall
                    )) ^ 0
	, _PrefixExpParens = V'(' *_* V'Exp' *_* CHECK(')', 'parenthesized expression')
	, _PrefixExpSquare = V'[' *_* V'Exp' *_* CHECK(']', 'index field')
	, _PrefixExpDot    = V'.' *_* V'ID'
	, _PrefixExpArgs   = V'Args'
	, _PrefixExpColon  = V':' *_* V'ID' *_* V'_PrefixExpArgs'

  -- solving the left recursion problem
	, Var          = V'_PrefixExp' * matchPrefix'Var'
	, FunctionCall = V'_PrefixExp' * matchPrefix'Call'

	-- FUNCTIONS
	, Function = V'FUNCTION' *_* V'FuncBody'
	, FuncBody = V'(' *_* (V'ParList'+V'EPSILON') *_* CHECK(')', 'parameter list')
             *_* V'Block' *_* CHECK('END', 'function body')
	, FuncName = V'Name' * (_* V'_PrefixExpDot')^0 
             * ((_* V':' *_* V'ID') + V'EPSILON')
	, Args     = V'(' *_* (V'ExpList'+V'EPSILON') *_* CHECK(')', 'argument list')
	           + V'TableConstructor' + V'STRING'
	, ParList  = V'NameList' * (_* V',' *_* V'...')^-1
	           + V'...'

	-- TABLE CONSTRUCTORS
	, TableConstructor = V'{' *_* (V'FieldList'+V'EPSILON') *_* CHECK('}', 'table constructor')

	, FieldList        = V'Field' * (_* V'FieldSep' *_* V'Field')^0 
                     * (_* V'FieldSep')^-1
	, Field            = V'_FieldSquare' + V'_FieldID' + V'_FieldExp'
	, _FieldSquare     = V'[' *_* V'Exp' *_* CHECK(']', 'index field') *_* CHECK('=', 'field assignment') *_* V'Exp'
	, _FieldID         = V'ID' *_* V'=' *_* V'Exp'
	, _FieldExp        = V'Exp'
	, FieldSep         = V',' + V';'

	-- OPERATORS
	, BinOp    = V'+'   + V'-'  + V'*' + V'/'  + V'^'  + V'%'  
             + V'..'  + V'<'  + V'<=' + V'>' + V'>=' + V'==' 
             + V'~='  + V'AND' + V'OR'
	, UnOp     = V'-' + V'NOT' + V'#'
  
  -- IDENTIFIERS
  , ID      = (AZ * (AZ + N)^0) - V'Keyword'
	, Keyword = anyOf(keywords)
  , Symbol  = anyOf(symbols)
}

-- add the Identifier rules
grammar.complete(rules, keywords)
grammar.complete(rules, symbols)

------------------------------ API --------------------------------------------

--[[
Uses [grammar.html#function_apply grammar.apply] to return a new grammar, with `captures` and extra rules. [#variable_rules rules] stays unmodified.

**Parameters:**
* `extraRules`: optional, the new and modified rules. See [grammar.html#function_apply grammar.apply] for the accepted format.
* `captures`: optional, the desired captures. See [grammar.html#function_apply grammar.apply] for the accepted format.

**Returns:**
* the extended grammar.
--]]
function apply(extraRules, captures)
  return grammar.apply(rules, extraRules, captures)
end

-- A pattern which matches any Lua identifier.
-- <% exp = 'LPeg pattern' %>
IDENTIFIER = P( apply(V'ID') )

-- A pattern which matches any Lua keyword.
-- <% exp = 'LPeg pattern' %>
KEYWORD = P( apply(V'Keyword') )

-- A pattern which matches any Lua symbol.
-- <% exp = 'LPeg pattern' %>
SYMBOL = P( apply(V'Symbol') )

-- Matches any Lua [#variable_IDENTIFIER identifier], [#variable_KEYWORD keyword], [#variable_SYMBOL symbol], [#variable_NUMBER number] or [#variable_STRING string].
-- <% exp = 'LPeg pattern' %>
TOKEN = IDENTIFIER + KEYWORD + SYMBOL + NUMBER + STRING

-- Matches any [#variable_TOKEN token], [#variable_COMMENT comment] or [#variable_SPACE space].
-- <% exp = 'LPeg pattern' %>
ANY = TOKEN + COMMENT + SPACE -- TEMP: + lexicalError'invalid character'

--[[
Checks if `input` is valid Lua source code.

**Parameters:**
* `input`: a string containing Lua source code.

**Returns:**
* `true`, if `input` is valid Lua source code, or `false` and an error message if the matching fails.
--]]
function check(input)
  local builder = grammar.pmatch(m.P(rules))
  local result, msg = builder:match(input) -- builder:match(input)
  
  if result then
    if result ~= #input + 1 then -- failure, build the error message
      local init, _ = rfind(input, '\n*', result - 1) 
      local _, finish = string.find(input, '\n*', result + 1)
      
      init = init or 0
      finish = finish or #input
      
      local line = lines(input:sub(1, result))
      local vicinity = input:sub(init + 1, finish)
      
      return false, 'Syntax error at line '..line..', near "'..vicinity..'"'
    end
  else
    return false, msg
  end
  
  return true
end

--[[
Strips all prefixing `--` and enclosing `--%[=%*%[` from comment tokens.

**Parameters:**
* `comment`: the comment to strip.

**Returns:**
* the text without comment marking syntax.
--]]
function comment2text (comment) -- TEMP: LPeg could be used here
	local ret, i, brackets = comment:find('^%-%-%[(=*)%[', i)
	
  if ret then
		comment = comment:gsub('^%-%-%['..brackets..'%[', '')  -- removes "--[===["
		comment = comment:gsub('%]'..brackets..'%]$', '')      -- removes "]===]"
		comment = '\n' .. comment
		comment = comment:gsub('\n\n', '\n%-%-\n')             -- adjust empty lines
		comment = comment:gsub('\n%s*%-%-+ ?', '\n' )          -- removes "--+ " prefix from lines
		comment = comment:gsub('^\n\n?', '')                   -- removes empty prefix lines
		comment = comment:gsub('\n$', '')                      -- removes empty sufix lines
	else
		comment = comment:gsub('^%-%-+%s*', '')
	end
  
	return comment
end

--[[
Encloses the text with comment markers.

**Parameters:**
* `text`: the text to comment.

**Returns:**
* the text with comment marking syntax.
--]]
function text2comment (text)
	-- searching for the largest [(=)*[ in the text
  local max = -1
  
  local updateMax = function (c) if max < #c then max = #c end end
  local openPatt = P'[' * m.C((P'=')^0) * P'[' / updateMax
  local closePatt = P']' * m.C((P'=')^0) * P']' / updateMax
  
  grammar.anywhere(openPatt):match(text)
  grammar.anywhere(closePatt):match(text)
  
  -- enclosing text with --[(=)^(max+1)[ and --](=)^(max + 1)]
  local equals = string.rep('=', max + 1)
	return '--['..equals..'[\n'..text..'--]'..equals..']'
end

-- used for escape processing in string2text
local escapeTable = {
  ['\\n'] = '\n',
  ['\\t'] = '\t',
  ['\\r'] = '\r',
  ['\\v'] = '\v',
  ['\\a'] = '\a',
  ['\\b'] = '\b',
  ['\\f'] = '\f',
  ['\\"'] = '"',
  ["\\'"] = "'",
  ['\\\\'] = '\\',
}

-- used for escape processing in text2string
local reverseEscapeTable = {}

for k, v in pairs(escapeTable) do
  reverseEscapeTable[v] = k
  reverseEscapeTable[string.byte(v)] = k
end

--[=[
Strips all enclosing `'`, `"`, and `%[=%*%[` from string tokens, and processes escape characters.

**Parameters:**
* `str`: the string to strip.

**Returns:**
* the text without string enclosers.
--]=]
function string2text(str)
  local escapeNum = m.C(N^-3) / tonumber
  local escapePatt = (
      (P'\\' * S[[ntrvabf'"\\]]) / escapeTable
    + (P'\\' * escapeNum) / string.char
  )
  
  local openDQuote, openQuote = P'"' / '', P"'" / ''
  local closeDQuote, closeQuote = openDQuote, openQuote
  
  local start, l = "[" * P"="^0 * "[", nil
  local longstring = #(P'[' * S'[=') * P(function (s, i)
    l = start:match(s, i)
    if not l then return nil end
    
    local p = P("]"..string.rep("=", l - i - 2).."]")
    p = (1 - p)^0 * p
    
    return p:match(s, l)
  end)
  
  
	local patt = Cs(
      (openDQuote * ((escapePatt + 1) - closeDQuote)^0 * closeDQuote)
    + (openQuote * ((escapePatt + 1) - closeQuote)^0 * closeQuote)
    + longstring / function (c) return string.sub(c, l, -l) end
  )
  
  return patt:match(str)
end

--[[
Transforms a text into a syntactically valid Lua string. Similar to `string.format` with the `'%%q'` option, but inserting escape numbers and escape codes where applicable.

**Parameters**
* `text`: a string containing the text.

**Returns:**
* a string, similar to string.format with option `'%%q'`.
--]]
function text2string(text)
  local PUNCTUATION = S'!@$&|?~`´'

  local function reverseEscape(char)
    local c = reverseEscapeTable[char]
    
    if c then 
      return c
    elseif (AZ + N + SPACE + SYMBOL + PUNCTUATION):match(char) then
      return char
    else
      return '\\'..string.byte(char)
    end
  end
  
  local escapePatt = P(1) / reverseEscape
  local patt = Cs(escapePatt^0)
  
  return '"'..patt:match(text)..'"'
end

