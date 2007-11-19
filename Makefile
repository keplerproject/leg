#
# Makefile for Leg
# $Id: Makefile,v 1.3 2007/11/19 13:56:47 hanjos Exp $
# 

# ===== SYSTEM PATHS ==============
# The system's executables directory
SYS_BIN = /usr/local/bin

# ===== LUA PATHS =================
# Lua's library directory
LUA_LIB = /usr/local/share/lua/5.1

# ===== PROJECT INFO ==============
# version
VERSION = v0.1

# project directories
SRC_DIR = src
TEST_DIR = test

# document generator
DOCCER = lua /usr/local/share/lua/5.1/ldt/ldt.lua

# project info
PROJ_NAME = leg

# ===== MISC ======================
#TIMESTAMP = `date +'%Y-%m-%d'`

# ===== RULES =====================

install:
  # copying the source files to LUA_LIB
	mkdir -p $(LUA_LIB)/$(PROJ_NAME)
	rm -f $(LUA_LIB)/$(PROJ_NAME)/*.lua
	cp src/*.lua $(LUA_LIB)/$(PROJ_NAME)

clean:
  # removing the source files and package
	rm -r $(LUA_LIB)/$(PROJ_NAME)

document:
	# generate documentation
	mkdir -p doc
	$(DOCCER) src/*.lua
	mv ldt/* doc
	rm -rf ldt

bundle:
  # tar-ing it
	tar cvf ../$(PROJ_NAME)_$(VERSION).tar $(SRC_DIR)/
	
  # zipping it
	gzip ../$(PROJ_NAME)_$(VERSION).tar

test:
	lua tests/test.lua
