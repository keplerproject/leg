#
# Makefile for Leg
# $Id: Makefile,v 1.5 2007/11/22 21:15:24 hanjos Exp $
# 

# ===== SYSTEM PATHS ==============
# The system's executables directory
SYS_BIN = /usr/local/bin

# ===== LUA PATHS =================
# Lua's library directory
LUA_LIB = /usr/local/share/lua/5.1

# ===== PROJECT INFO ==============
# version
VERSION = 0.1.1

# project directories
SRC_DIR = src
TEST_DIR = tests

# document generator
DOCCER = lua /usr/local/share/lua/5.1/ldt/ldt.lua

# project info
PROJ_NAME = leg

# ===== MISC ======================
#TIMESTAMP = `date +'%Y-%m-%d'`

# ===== RULES =====================
leg:
	mkdir -p $(PROJ_NAME)
	rm -f $(PROJ_NAME)/*.lua
	cp src/*.lua $(PROJ_NAME)

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
  # ugly trick, but it works
	mkdir -p ../$(PROJ_NAME)-$(VERSION)
	rm -rf ../$(PROJ_NAME)-$(VERSION)/*
	cp -r * ../$(PROJ_NAME)-$(VERSION)

	# tar-ing it (this works only with version 1.19 and beyond)
	tar --create --verbose --exclude-vcs --file=../$(PROJ_NAME)-$(VERSION).tar ../$(PROJ_NAME)-$(VERSION)
	
  # zipping it
	gzip ../$(PROJ_NAME)-$(VERSION).tar

	# removing the buffer directory
	rm -r ../$(PROJ_NAME)-$(VERSION)

test:
	@echo
	@echo '==================== SCANNER ===================='
	@echo
	@lua $(TEST_DIR)/test_scanner.lua
	@echo
	@echo '==================== PARSER ===================='
	@echo
	@lua $(TEST_DIR)/test_parser.lua
	@echo
	@echo '==================== DONE! ===================='
