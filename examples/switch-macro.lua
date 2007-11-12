-------------------------------------------------------------------------------
-- 
-- A preprocessor which transforms a switch construct (called 'match' here) 
-- into a sequence of if-elseif blocks. There is quite a discussion going on
-- (again...) about adding switch statements in Lua 
-- (http://lua-users.org/lists/lua-l/2007-11/msg00099.html), so I decided to 
-- use it as a demonstration of Leg's capabilities.
--
-- Author: Humberto Anjos
-- 
-- $Id: switch-macro.lua,v 1.1 2007/11/12 20:32:01 hanjos Exp $
-- 
-------------------------------------------------------------------------------

-- imports
local lpeg = require 'lpeg'

local parser = require 'leg.parser'

-- aliasing
local P, V, C, Ct, Cs = lpeg.P, lpeg.V, lpeg.C, lpeg.Ct, lpeg.Cs

-- command-line arguments
local args = { ... }

-- the code to parse
subject = args[1] or [=[
  match assert(io.open(file, '*a'))
    -- no cases here, nothing is generated
  end
  
  local args = { ... }
  
  -- before match
  match #args
    -- before the first case
    when 0 do print 'No arguments!'
    -- before the second case
    when 1 do -- after case checking
      print('only one argument: ', args[1])
    when 2 do
      print('two arguments: ', args[1], args[2])
      
      local a = tonumber(args[1]) + tonumber(args[2])
      io.write(a) io.flush()
    else -- before else block
      for _, v in ipairs(args) do
        print(v)
      end
    -- after else
  end
  -- after match
  
  function eval(node, ...)
    match node.tag
      when 'Const' do 
        return node.value
      when (node.left and node.right) and 'BinOp' do 
        -- this comment won't appear in the generated code
        local op, left, right = node.op, node.left, node.right
        -- but this one will
        return op(left, right)
        -- and this one too
      when node.operand and 'UnOp' do
        local op, operand = node.op, node.operand
        
        return op(operand)
      else
        match isList(node) -- internal match statement
          when true do visit(node)
          when false do error 'Invalid node!'
        end
    end
  end
]=]

-- After reading several proposals on the Lua discussion list, I decided to 
-- implement the following one:
-- 
-- match <exp>
--   when <case1> do <block1>
--   when <case2> do <block2>
--   ...
--   else <block_else>
-- end
-- 
-- with <case1>, <case2>, ... <casen> are Lua expressions.
-- 
-- which will be converted into the following code:
--
-- do
--   local __temp__ = <exp>
--   if __temp__ == (<case1>) then <block1>
--   elseif __temp__ == (<case2>) then <block2>
--   ...
--   else <block_else> end
-- end
-- 
-- Notes:
-- 
-- * Technically, the local variable __temp__ should receive a name provably 
-- unique in the program. But, for this example, naming it __temp__ and 
-- restricting its scope does the trick.
-- * If there's only the default case, its block will be the sole result.
-- * If there's no cases and no default case, the if-elseif blocks will not be 
-- generated.
-- * About comment capturing: some comments are captured, some are not. The 
-- comments captured are those which are in the middle or at the end of a 
-- <block> statement, being captured along with the block.

-- spacing
local S = V'IGNORED' -- parser.rules.IGNORED or scanner.IGNORED could be used

-- epsilon
local EPSILON = V'EPSILON' / function () end

-- new matching rule
local Match = (P'match' *S* C(V'Exp') *S*
              Ct((P'when' *S* C(V'Exp') *S* P'do' *S* V'Block')^0) *S*
              ((P'else' *S* V'Block') + EPSILON) *S* P'end')
              / function (exp, cases, default)
                if #cases == 0 then
                  if default then
                    return 'do local __temp__ = ('..exp..') '..default..' end'
                  else
                    return ''
                  end
                else
                  local str = 'do local __temp__ = ('..exp..') '
                  
                  for i = 1, #cases - 3, 2 do
                    str = str..'if __temp__ == ('..cases[i]..') then '..cases[i + 1]..' else'
                  end
                  
                  -- the last case
                  str = str..'if __temp__ == ('..cases[#cases - 1]..') then '..cases[#cases]
                  if default then
                    str = str..' else '..default..' end'
                  else
                    str = str..' end' -- end if
                  end
                  
                  return str..' end' -- end do
                end
              end

-- creating the LPeg pattern
local oldStat, oldBlock = parser.rules.Stat, parser.rules.Block


local MATCH = P( parser.apply { 
  Stat = oldStat + Match, -- adding Match to the list of possible Statements
  Block = Cs(oldBlock) -- the Block rule needs to be updated as well, in order to 
                       -- make the necessary substitutions to inner Match statements
} )

print('subject:', '\n'..subject)
print('result:', '\n'..MATCH:match(subject))