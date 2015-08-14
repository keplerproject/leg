-------------------------------------------------------------------------------
-- 
-- An AST builder for Leg. This AST is from Metalua 
-- (http://metalua.luaforge.net).
--
-- Author: Humberto Anjos (the code below) and Fabien Fleutot (the AST design)
-- 
-- $Id: metalua-ast.lua,v 1.3 2007/12/07 14:23:56 hanjos Exp $
-- 
-------------------------------------------------------------------------------

-- basic modules
local _G      = _G
local math    = math
local string  = string
local table   = table

-- basic functions
local error     = error
local ipairs    = ipairs
local pairs     = pairs
local print     = print
local require   = require
local select    = select
local tonumber  = tonumber
local type      = type
local unpack    = unpack

-- imported modules
local parser  = require 'leg.parser'
local grammar = require 'leg.grammar'

local lpeg = require 'lpeg'

-- imported functions
local P = lpeg.P

-- AST BUILDING FUNCTIONS -------------

-- the table holding the node builders
builder = {} 

-- last stats
function builder.Break() return { tag = 'Break' } end
function builder.Return(...) return { tag = 'Return', ... } end

-- statements
function builder.Do(block) return { tag = 'Do', block } end
function builder.Let(lhslist, exprlist) 
  return { tag = 'Let', lhslist, exprlist } 
end

function builder.While(expr, block) return { tag = 'While', expr, block } end
function builder.Repeat(block, expr) return { tag = 'Repeat', block, expr } end
function builder.If(...) return { tag = 'If', ... } end

function builder.Fornum(var, index, limit, step, block) 
  return { tag = 'Fornum', var, index, limit, step or Number(1), block } 
end

function builder.Forin(varlist, exprlist, block) 
  return { tag = 'Forin', varlist, exprlist, block } 
end

function builder.Local(varlist, exprlist) 
  return { tag = 'Local', varlist, exprlist } 
end

function builder.Localrec(varlist, exprlist) 
  return { tag = 'Localrec', varlist, exprlist }
end

function builder.Call(func, ...) return { tag = 'Call', func, ... } end
function builder.Method(table, string, ...) 
  return { tag = 'Method', table, string, ... } 
end

-- expressions
function builder.Nil() return { tag = 'Nil' } end
function builder.Dots() return { tag = 'Dots' } end
function builder.True() return { tag = 'True' } end
function builder.False() return { tag = 'False' } end
function builder.Number(number) return { tag = 'Number', number } end
function builder.String(string) return { tag = 'String', string } end

function builder.Function(parlist, block) 
  return { tag = 'Function', parlist, block } 
end

function builder.Table(...) return { tag = 'Table', ... } end
function builder.Key(key, value) return { tag = 'Key', key, value } end

function builder.Op(op, value1, value2) 
  return { tag = 'Op', op, value1, value2 } 
end

-- a parenthesized expression
function builder.One(expr) return { tag = 'One', expr } end

-- variables
function builder.Id(identifier) return { tag = 'Id', identifier } end
function builder.Index(table, index) return { tag = 'Index', table, index } end

-- operators
function builder.Add()    return { tag = 'Add' } end
function builder.Sub()    return { tag = 'Sub' } end
function builder.Mul()    return { tag = 'Mul' } end
function builder.Div()    return { tag = 'Div' } end
function builder.Mod()    return { tag = 'Mod' } end
function builder.Pow()    return { tag = 'Pow' } end
function builder.Concat() return { tag = 'Concat' } end
function builder.Eq()     return { tag = 'Eq' } end
function builder.Ne()     return { tag = 'Ne' } end
function builder.Gt()     return { tag = 'Gt' } end
function builder.Ge()     return { tag = 'Ge' } end
function builder.Lt()     return { tag = 'Lt' } end
function builder.Le()     return { tag = 'Le' } end
function builder.And()    return { tag = 'And' } end
function builder.Or()     return { tag = 'Or' } end
function builder.Not()    return { tag = 'Not' } end
function builder.Len()    return { tag = 'Len' } end

-- technically, the operator Sub is also used for the unary operator -,
-- but to avoid ambiguities during the construction of the expression tree,
-- I preferred to build an Unm node and change it to a Sub when the node
-- is safely identified as an unary -.
function builder.Unm()    return { tag = 'Unm' } end

-- OPERATOR PROCESSING CODE -----------

-- OBS.:
-- Leg's grammar does not specify operator precedence, so it must be treated 
-- outside the grammar. This really sucks, and it's on the list of things to 
-- improve in future versions.

-- Operator precedence table. Maps operator tags to a table holding the 
-- respective precedence, left or right associativity, and arity (unary 
-- or binary)
local ops = {
  Or      = { precedence = 1, left  = true, arity = 2 },
  And     = { precedence = 2, left  = true, arity = 2 },
  Eq      = { precedence = 3, left  = true, arity = 2 },
  Ne      = { precedence = 3, left  = true, arity = 2 },
  Le      = { precedence = 3, left  = true, arity = 2 },
  Ge      = { precedence = 3, left  = true, arity = 2 },
  Lt      = { precedence = 3, left  = true, arity = 2 },
  Gt      = { precedence = 3, left  = true, arity = 2 },
  Concat  = { precedence = 4, right = true, arity = 2 },
  Add     = { precedence = 5, left  = true, arity = 2 },
  Sub     = { precedence = 5, left  = true, arity = 2 },
  Mul     = { precedence = 6, left  = true, arity = 2 },
  Div     = { precedence = 6, left  = true, arity = 2 },
  Mod     = { precedence = 6, left  = true, arity = 2 },
  Not     = { precedence = 7,               arity = 1 },
  Len     = { precedence = 7,               arity = 1 },
  Unm     = { precedence = 7,               arity = 1 },
  Pow     = { precedence = 8, right = true, arity = 2 }
}

-- some self-explaining helper functions
local function isOperator(node)
  return node and ops[node.tag] 
end

local function isUnary(node)
  return isOperator(node) and ops[node.tag].arity == 1
end

local function isBinary(node)
  return isOperator(node) and ops[node.tag].arity == 2
end


-- Takes a list of tokens with Lua values and operators and returns it in 
-- Reverse Polish Notation, using Dijkstra's shunting yard algorithm. The 
-- actual expression tree will be built in Exp's capture function
local function toRPN(list)
  local queue = {}
  local stack = {}
  
  for _, v in ipairs(list) do
    if isBinary(v) or isUnary(v) then
      local vPrec, topPrec
      
      if stack[#stack] then
        vPrec, topPrec = ops[v.tag].precedence, 
                         ops[stack[#stack][1].tag].precedence
      end
      
      while stack[#stack] and ((ops[v.tag].right and vPrec < topPrec)
        or (ops[v.tag].left and vPrec <= topPrec)) do
        
        queue[#queue + 1] = table.remove(stack)
      end
      
      stack[#stack + 1] = builder.Op(v)
    else
      queue[#queue + 1] = v
    end
  end
  
  -- dumping the stack
  for i = #stack, 1, -1 do
    queue[#queue + 1] = stack[i]
  end
  
  return queue
end

-- a temporary node
local function MethodDecl(Index, Method)
  return { tag = 'MethodDecl', Index, Method }
end

-- a temporary node
local hole = { tag = 'Hole' }

-- a table mapping an operator to its builder function
local opToBuilder = {
  ['or']  = builder.Or,
  ['and'] = builder.And,
  ['==']  = builder.Eq,
  ['~=']  = builder.Ne,
  ['<=']  = builder.Le,
  ['>=']  = builder.Ge,
  ['<']   = builder.Lt,
  ['>']   = builder.Gt,
  ['..']  = builder.Concat,
  ['+']   = builder.Add,
  ['-']   = builder.Sub,
  ['*']   = builder.Mul,
  ['/']   = builder.Div,
  ['%']   = builder.Mod,
  ['not'] = builder.Not,
  ['#']   = builder.Len,
  ['unm'] = builder.Unm,
  ['^']   = builder.Pow,
}

-- CAPTURE TABLE ----------------------

-- the capture table. This table will be piped to parser.rules to build the
-- AST.
captures = {
  -- Statements
  Block = function (...)
    local Block = { ... }
    
    -- if the captured block has no statements, Block will contain { '' }.
    -- Detect that and return an empty table in that case
    if #Block == 1 and Block[1] == '' then
      return {}
    end
    
    return Block
  end,
  
  Assign      = builder.Let,
  Do          = builder.Do,
  While       = builder.While,
  Repeat      = builder.Repeat,
  If          = builder.If,
  NumericFor  = builder.Fornum,
  GenericFor  = builder.Forin,
  
  GlobalFunction = function (FuncName, FuncBody)
    if FuncName.tag == 'MethodDecl' then -- it's a method declaration
      -- a method declaration like 'function b:o() <...> end' is equivalent to
      -- 'b.o = function (self) <...> end'
    
      FuncName.tag = 'Index' -- FuncName should be an Index node then
      
      local parlist = FuncBody[1]
      table.insert(parlist, 1, builder.Id 'self')
    end
    
    return builder.Let( { FuncName }, { FuncBody } )
	end,
  
  LocalFunction = function (Name, FuncBody)
    return builder.Localrec( { Name }, { FuncBody })
	end,
  
  LocalAssign = function (NameList, ExpList)
    return builder.Local(NameList, ExpList or {})
  end,
  
  LastStat = function (STAT)
		if STAT == 'break' then
			return builder.Break()
		else
			if STAT == 'return' then
				STAT = {}
			end
      
			return builder.Return(unpack(STAT))
		end
	end,
  
  -- Expressions
  
  -- Takes a list of tokens and operators and builds the appropriate tree node
  Exp = function (...) 
    local list = { ... }
    
    if #list == 1 then 
      return list[1] 
    end
    
    local listRPN = toRPN(list) -- putting the list in RPN
    local stack = {}
    
    for _, v in ipairs(listRPN) do
      if v.tag == 'Op' and isUnary(v[1]) and not v[2] then
        if v[1].tag == 'Unm' then -- replacing Unm with Sub
          v[1].tag = 'Sub'
        end
        
        v[2] = table.remove(stack)
      elseif v.tag == 'Op' and isBinary(v[1]) and not v[2] and not v[3] then
        v[3] = table.remove(stack)
        v[2] = table.remove(stack)
      end
      
      stack[#stack + 1] = v
    end
    
    return stack[1]
  end,
  
  _PrefixExp = function (base, ...)
    for _, suffix in ipairs { ... } do
      -- filling the holes
      suffix[1] = base
			base = suffix
		end
    
		return base
	end,
  
  _PrefixExpParens = function (Exp)
		return builder.One(Exp)
	end,
	
	_PrefixExpDot   = function (ID)
    -- the hole will be filled in _PrefixExp
		return builder.Index(hole, builder.String(ID))
	end,
	
  _PrefixExpSquare = function (Exp)
    -- the hole will be filled in _PrefixExp
		return builder.Index(hole, Exp) 
  end,
  
	_PrefixExpColon = function (ID, _PrefixExpArgs)
		-- the hole will be filled in _PrefixExp
    return builder.Method(hole, builder.String(ID), 
      select(2, unpack(_PrefixExpArgs)))
	end,
	
  _PrefixExpArgs  = function (Args)
		-- the hole will be filled in _PrefixExp
		return builder.Call(hole, unpack(Args))
	end,

  -- Functions and closures
  FuncName = function (Name, ...)
		local base = Name
		
    for _, v in ipairs {...} do
      if type(v) == 'string' then -- it's a method
        -- using MethodDecl; this will be transformed into an Index node later
        base = MethodDecl(base, builder.String(v))
        
      elseif v.tag == 'Index' then
        v[1] = base
        
        base = v
      end
    end
    
		return base
	end,
  
  FuncBody = function (ParList, Block)
    return builder.Function(ParList or {}, Block)
  end,
  
  Args = function (arg)
		if (not arg) or arg.tag then -- there's either one or no arguments
			arg = { arg }
		end
    
		return arg
	end,
  
  -- Lists
  VarList   = grammar.Ct,
  NameList  = grammar.Ct,
  ExpList   = grammar.Ct,
  
  ParList = function (NameList, varargs)
    if NameList.tag == 'Dots' then -- the parameter list is just ...
      return { NameList }
    end
    
    NameList[#NameList + 1] = varargs
    return NameList
  end,
  
  -- Table constructors
  TableConstructor = function (FieldList)
    FieldList = FieldList or {}
		
    return builder.Table(unpack(FieldList))
  end,
  
  -- fields
  FieldList     = grammar.Ct,
  _FieldSquare  = builder.Key,
  _FieldExp     = grammar.C,
  
  _FieldID = function (ID, Exp) 
    return builder.Key(builder.String(ID), Exp) 
  end,
  
  -- Operators
  BinOp = function (op) return opToBuilder[op]() end,
	UnOp = function (op) 
    if op == '-' then
      return opToBuilder['unm']() 
    else
      return opToBuilder[op]() 
    end
  end,
  
  -- Simple expressions
  NIL     = builder.Nil,
  TRUE    = builder.True,
  FALSE   = builder.False,
  NUMBER  = function (num) return builder.Number(tonumber(num)) end,
  STRING  = function (str) return builder.String(parser.string2text(str)) end,
  ID      = grammar.C,
  Name    = builder.Id,
  ['...'] = builder.Dots,
  
  -- Helper patterns
  EPSILON = function () return nil end,
}

-- the matching pattern
local patt = P( parser.apply(nil, captures) )

-- Takes a string and checks if it's syntactically valid Lua 5.1 code. If it
-- is, the corresponding AST is built and returned; if not, an error is thrown.
function build(input)
  local result, msg = parser.check(input) 
  
  if result then return patt:match(input)
  else error(msg) end
end

-- shamelessly stolen from Metalua: this is its table.tostring function, slightly
-- adapted to substitute its dependencies for my own code.
local function ast2string(t, ...)
  local LINE_MAX, PRINT_HASH = math.huge, true
  for _, x in ipairs {...} do
    if type(x) == "number" then LINE_MAX = x
    elseif x=="nohash" then PRINT_HASH = false
    end
  end

  local current_offset =  0  -- indentation level
  local xlen_cache     = { } -- cached results for xlen()
  local acc_list       = { } -- Generated bits of string
  local function acc(...)    -- Accumulate a bit of string
    local x = table.concat{...}
    current_offset = current_offset + #x
    table.insert(acc_list, x) 
  end
  local function valid_id(x)
    -- FIXME: we should also reject keywords.
    return type(x) == "string" and parser.IDENTIFIER:match(x)
  end
  local function shallowcopy(t)
    local newt = {}
    
    for k, v in pairs(t) do
      newt[k] = v
    end
    
    return newt
  end

  -- Compute the number of chars it would require to display the table
  -- as a single line. Helps to decide whether some carriage returns are
  -- required. Since the size of each sub-table is required many times,
  -- it's cached in [xlen_cache].
  local xlen_type = { }
  local function xlen(x, tracker)
    tracker = tracker or { }
    if x==nil then return #"nil" end
    if tracker[x] then return #_G.tostring(x) end
    local len = xlen_cache[x]
    if len then return len end
    local f = xlen_type[type(x)]
    if not f then return #_G.tostring(x) end
    len = f (x, tracker) 
    xlen_cache[x] = len
    return len
  end

  -- optim: no need to compute lengths if I'm not going to use them
  -- anyway.
  if LINE_MAX == math.huge then xlen = function() return 0 end end

  xlen_type["nil"] = function() return 3 end
  function xlen_type.number(x)  return #_G.tostring(x) end
  function xlen_type.boolean(x) return x and 4 or 5 end
  function xlen_type.string(x)  return #string.format("%q",x) end
  function xlen_type.table (adt, tracker)

    -- Circular references detection
    tracker = shallowcopy(tracker)
    tracker [adt]  = true 

    local has_tag  = valid_id(adt.tag)
    local alen     = #adt
    local has_arr  = alen>0
    local has_hash = false
    local x = 0
    
    if PRINT_HASH then
       -- first pass: count hash-part
       for k, v in pairs(adt) do
          if k=="tag" and has_tag then 
             -- this is the tag -> do nothing!
          elseif type(k)=="number" and k<=alen and math.fmod(k,1)==0 then 
             -- array-part pair -> do nothing!
          else
             has_hash = true
             if valid_id(k) then x=x+#k
             else x = x + xlen (k, tracker) + 2 end -- count surrounding barckets
             x = x + xlen (v, tracker) + 5          -- count " = " and ", "
          end
       end
    end

    for i = 1, alen do x = x + xlen (adt[i], tracker) + 2 end -- count ", "
    
    if not (has_tag or has_arr or has_hash) then return 3 end
    if has_tag then x=x+#adt.tag+1 end
    if not (has_arr or has_hash) then return x end
    if not has_hash and alen==1 and type(adt[1])~="table" then
       return x-2 -- substract extraneous ", "
    end
    return x+2 -- count "{ " and " }", substract extraneous ", "
  end

  -- Recursively print a (sub) table at given indentation level.
  -- [newline] indicates whether newlines should be inserted.
  local function rec (adt, indent, tracker)
    local function acc_newline()
       acc ("\n"); acc (string.rep (" ", indent)) 
       current_offset = indent
    end
    local x = { }
    x["nil"] = function() acc "nil" end
    function x.number()   acc (_G.tostring (adt)) end
    function x.string()   acc (string.format ("%q", adt)) end
    function x.boolean()  acc (adt and "true" or "false") end
    function x.table()
       tracker[adt]   = true
       local has_tag  = valid_id(adt.tag)
       local alen     = #adt
       local has_arr  = alen>0
       local has_hash = false
       local new_indent
       if has_tag then acc("`"); acc(adt.tag) end

       -- First pass: handle hash-part
       if PRINT_HASH then
          for k, v in pairs(adt) do
             if k=="tag" and has_tag then -- this is the tag -> do nothing!
             elseif type(k)=="number" and k<=alen and math.fmod(k,1)==0 then
                -- nothing: this an array-part pair, parsed later
             else  -- hash-part pair

                -- Is it the first time we parse a hash pair?
                if not has_hash then acc "{ "; indent = current_offset
                else acc ", " end

                -- Determine whether a newline is required
                local is_id, expected_len = valid_id(k)
                if is_id then expected_len = #k + xlen (v, tracker) + #" = , "
                else expected_len = xlen (k, tracker) + 
                                    xlen (v, tracker) + #"[] = , " end
                if has_hash and expected_len + current_offset > LINE_MAX
                then acc_newline() end
                
                -- Print the key
                if is_id then acc(k); acc " = " 
                else  acc "["; rec (k, current_offset, tracker); acc "] = " end

                -- Print the value
                rec (v, current_offset, tracker)
                has_hash = true
             end
          end
       end

       -- now we know whether there's a hash-part, an array-part, and a tag.
       -- Tag and hash-part are already printed if they're present.
       if not has_tag and not has_hash and not has_arr then acc "{ }"; return
       elseif has_tag and not has_hash and not has_arr then return -- nothing!
       else -- has_hash or has_arr
          if has_hash and has_arr then acc ", " 
          elseif has_tag and not has_hash and alen==1 and type(adt[1])~="table" then
            -- No brace required; don't print "{" and return before printing "}"
            acc (" "); rec (adt[1], new_indent, tracker); return
          elseif not has_hash then
            -- Braces required, but not opened by hash-part handler yet
            acc "{ "; indent = current_offset 
          end

          -- 2nd pass: array-part
          if has_arr then 
            rec (adt[1], new_indent, tracker)
            for i=2, alen do 
              acc ", ";                   
              if   current_offset + xlen (adt[i], { }) > LINE_MAX
              then acc_newline() end
              rec (adt[i], new_indent, tracker) 
            end
          end
          acc " }"
       end
    end
    local y = x[type(adt)]
    if y then y() else acc(_G.tostring(adt)) end
  end
  rec(t, 0, { })
  return table.concat (acc_list)
end

-- TESTING ----------------------------
local args = { ... }

subject = args[1] or [=[
  -- this comment won't be captured
  local a = 3 + -math.pi
  
  function b(ascii, ...)
    local t = { a = 1, ['b'] = {}, -3, .234 }
    
    while _VERSION > 5.1 do
      if x['a'] then
        x = false
        return x, 1, 2, 3, -4
      else
        break
      end
    end
    
  end
  
  local function ascii2() return [[ascii2!]], whatever end
]=]

print('subject:', '\n'..subject)
print('result:', '\n'..ast2string(build(subject), 80, 'nohash'))