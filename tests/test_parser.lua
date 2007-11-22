-------------------------------------------------------------------------------
-- Parser test suite for Leg
--
-- Authors: Humberto Anjos and Francisco Sant'Anna
-- Copyright (c) 2007 Leg
--
-- $Id: test_parser.lua,v 1.2 2007/11/22 21:15:24 hanjos Exp $
-- 
-------------------------------------------------------------------------------

local lpeg = require 'lpeg'
local parser = require 'leg.parser'

local function TEST (rule, tests)
	io.write(string.format("%-26s", 'Testing '..rule..': ')) io.flush()
	local G = lpeg.P( parser.apply(lpeg.V(rule)) )
	
	for i, subject in ipairs(tests) do
		io.write(i..'... ') io.flush()
		
		if string.sub(subject, 1, 1) == '!' then
			subject = string.sub(subject, 2)
			
			local G = G * function(subject) print() error('"'..subject..'": should not match') end
			G:match(subject)
		else
			local G = G + function(subject) print() error('"'..subject..'": should match')     end
			G:match(subject)
		end
	end
	
	print'OK!'
end

TEST('UnOp', { '-', 'not', '#', '!--', '!anot', '!nota' })
TEST('BinOp', { '+', '-', '*', '/', '^', '%', '..', '<', '<=', '>', '>=', '==', '~=', 'and', 'or' })
TEST('FieldSep', { ',', ';' })
TEST('Field', { '[a] = f(1)', 'a.name().name', 'name = 1' })
TEST('FieldList', { '[a] = f(1), a.name().name, name = 1,' })
TEST('TableConstructor', { "{ a, b, c, c=c, [1]={},  }", "{ ['oi']=nil, 'as', true, false, [{}]={}}" })
TEST('ParList', { 'a, d, ...', '...', '..., a' })
TEST('FuncBody', { '(a, ...) a = {1, 2, 3} + {} - ac end', '(o) return a end' })
TEST('Function', { 'function () return { a = "a" + 10 } end' })
TEST('Args', { "(a+1, {}, 'oi' )", '{a,c,s}', "'aaa'", '( function() a=1 end )' })
TEST('FunctionCall', { 'a()', 'a.name(2)', 'a.name:method()', 'print( function() a=1 end )' })
TEST('_PrefixExp', { 'a', '(a.name)', '((aaa))' })
TEST('_PrefixExp', { 'a', 'a.name', 'a.name:method()', '(a+1):x()', '(a[1+{}]:method())()', '(a[1+{}]:method())().abs', 'print( function() print() end )' })
TEST('_SimpleExp', { 'nil', 'false', 'true', '1e-10', '"adkaljsdaldk"', '...', 'function() return a end', '(a[1+{}]:method())().abs', '{}' })
TEST('Exp', { '1 + 1', '2*5-(10-f().name):a()+13', '-1', '-1*2*4^0', 'function() print() end', '1 * {} + -9 ^ 1-1', [[(a[1+{}]:method())().abs]] })
TEST('ExpList', { 'a, b, c', 'a+1, f:name(1+2), -a', })
TEST('NameList', { 'a, b, c' })
TEST('Var', { '!(a)', '!a()', 'a[a]', 'a.name', 'a.name[1]', '(a+1).name', '(a[1+{}]:method())()[f()]', '(a[1+{}]:method())().abs', })
TEST('VarList', { '!(a), b(), c:c()', 'a, b, b[1], (b())[1]' })
TEST('FuncName', { 'f.name.aa.a.a.a', 'f.name.name.name:aa', })
TEST('LastStat', { 'return', 'return f(1):a()', })

TEST('Assign',        { 'a, b, c = a+1, f:name(1+2), -a', 'a=a()-1', '!subject  _G.assert(io.open(input))' })
TEST('FunctionCall',      { 'a()', '(a+1)(b+ c)(2^0)' })
TEST('Do',            { 'do a=1 b=f(a(),b()) break end' })
TEST('While',         { 'while true do return 1 end' })
TEST('Repeat',        { 'repeat a=1 until false' })
TEST('If',            { 'if a == 1 then a=1 end', 'if 1 then return a elseif 2 then return b else return c end', 'if g() then repeat a=1 until false end' })
TEST('NumericFor',    { 'for a=f(), g()+1/2, -100e-10 do a=1 end' , 'for a=1, 10 do if a then a = 1 else break end end' })
TEST('GenericFor',    { 'for a, b, c in ipairs(a) do print(a, b, c) end' })
TEST('GlobalFunction',      { 'function a (a, b, ...) for k=1, 10 do end return true end', "function tofile (t, name, filename, opts) local fs = _G.require 'fs' return fs.value(t, name, filename, opts) end" })
TEST('LocalFunction', { 'local function a () end' })
TEST('LocalAssign',   { 'local a, b, as', 'local a, c = 1, 2, 3, f()', 'local a' })
TEST('Block', { 'b= a()' })
TEST('Chunk', { 'a=a', '', 'a, b, c=1', 'a=1; b=5; if a then return b end; break;' })


