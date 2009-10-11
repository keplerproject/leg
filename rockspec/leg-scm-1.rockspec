-- Package metadata
package = 'Leg'
version = 'scm-1'
description = {
  summary = 'A Lua 5.1 grammar',
  detailed = [[
    Leg offers a complete Lua 5.1 grammar, 
    along with a small API for user manipulation.
  ]],
  license = 'MIT/X11',
  homepage = 'http://leg.luaforge.net/',  
}

-- Dependency information
dependencies = {
  'lpeg >= 0.6',
  'lua >= 5.1',
}

-- Build rules
source = {
  url = 'git://github.com/keplerproject/leg.git',
}

build = {
  type = 'make',
  install_variables = {
    LUA_LIB = "$(LUADIR)",
  }
}
