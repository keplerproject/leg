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
-- $Id: switch-macro.lua,v 1.2 2007/11/19 13:34:47 hanjos Exp $
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
-- implement this one:
-- 
-- match <exp>
--   when <case 1> do <block 1>
--   when <case 2> do <block 2>
--   ...
--   when <case n> do <block n>
--   else <block else>
-- end
-- 
-- where <case 1>, <case 2>, ... <case n> are Lua expressions.
-- 
-- The construct above will be converted into the following code:
--
-- do
--   local __temp__ = <exp>
--   if __temp__ == (<case 1>) then <block 1>
--   elseif __temp__ == (<case 2>) then <block 2>
--   ...
--   elseif __temp__ == (<case n>) then <block n>
--   else <block else> end
-- end
-- 
-- Implementation notes:
-- 
-- * Technically, the local variable __temp__ should receive a name provably 
--   unique in the program. But, for this example, naming it __temp__ and 
--   restricting its scope will do the trick.
-- 
-- * The local declaration will always be generated, even if there are no 
--   clauses to match. This is done because the expression in the local 
--   declaration might have side effects, which affect program semantics even
--   if no match is made.
-- 
-- * The default case, if present, must be the last clause.
-- 
-- * If there's only the default case, the local declaration and the default 
--   case's block will be generated, without an enclosing if statement.
-- 
-- * If there are no cases and no default case, only the local declaration will
--   be generated. 
-- 
-- * Some comments are captured, some are not. The comments captured are those
--   which are in the middle or at the end of a <block> statement, and are 
--   captured along with the block. The other ones are matched as part of the 
--   spacing, and consequently not captured.
-- 
-- * This is an obvious one, but: since the result is a series of if-elseif 
--   blocks, there is no fallthrough.
-- 
-- * A reasonable improvement would be allowing a case clause to have several 
--   possible matches, generating something like 
--   if __temp__ == (<case 1>) or __temp__ == (<case 2>) then <block n> ...
-- 
--   This is left as an exercise to the reader *shameless cop-out*.

-- spacing
local S = V'IGNORED' -- parser.rules.IGNORED or scanner.IGNORED could be used

-- epsilon rule
local EPSILON = V'EPSILON' / function () end

-- new matching rule. Notice that the Block rule has no captures.
local Match = (P'match' *S* C(V'Exp') *S*
              Ct((P'when' *S* C(V'Exp') *S* P'do' *S* V'Block')^0) *S*
              ((P'else' *S* V'Block') + EPSILON) *S* P'end')
              / function (exp, cases, default)
                if #cases == 0 then -- no case clauses
                  if default then -- return the local declaration and the block
                    return 'do local __temp__ = ('..exp..') '..default..' end'
                  else -- generate just the local declaration
                    return 'do local __temp__ = ('..exp..') end'
                  end
                else -- there's at least one clause
                  local str = 'do local __temp__ = ('..exp..') '
                  
                  -- generating a new if or elseif block
                  for i = 1, #cases - 3, 2 do
                    str = str..'if __temp__ == ('..cases[i]..') then '
                      ..cases[i + 1]..' else'
                  end
                  
                  -- the last case clause
                  str = str..'if __temp__ == ('..cases[#cases - 1]..') then '
                    ..cases[#cases]
                    
                  if default then -- generate the else block
                    str = str..' else '..default..' end'
                  else -- no else, just finish it
                    str = str..' end' -- end if-elseif chain
                  end
                  
                  return str..' end' -- end do
                end
              end

-- creating the LPeg pattern
local oldStat, oldBlock = parser.rules.Stat, parser.rules.Block

local MATCH = P( parser.apply { 
  -- adding Match to the list of valid Statements
  Stat = oldStat + Match, 
  
  -- the Block rule needs to be updated as well, in order to make the 
  -- necessary substitutions to inner Match statements
  Block = Cs(oldBlock)
} )

print('subject:', '\n'..subject)
print('result:', '\n'..MATCH:match(subject))