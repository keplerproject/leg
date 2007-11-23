#
# Makefile for Leg
# $Id: Makefile,v 1.6 2007/11/23 18:15:42 hanjos Exp $
# 

# ===== LUA PATHS =================
# Lua's library directory
LUA_LIB = /usr/local/share/lua/5.1

# ===== PROJECT INFO ==============
# project info
NAME = leg
VERSION = 0.1.2

# project directories
DOC_DIR = doc
SRC_DIR = src
TEST_DIR = tests

# the document generator used to build the documentation in doc/.
# It uses an unreleased generator, so this is for my personal use.
DOCCER = lua /usr/local/share/lua/5.1/ldt/ldt.lua

# ===== RULES =====================
leg:
	mkdir -p $(NAME)
	rm -f $(NAME)/*.lua
	cp src/*.lua $(NAME)

install:
  # copying the source files to LUA_LIB
	mkdir -p $(LUA_LIB)/$(NAME)
	rm -f $(LUA_LIB)/$(NAME)/*.lua
	cp src/*.lua $(LUA_LIB)/$(NAME)

clean:
  # removing the source files and package
	rm -r $(LUA_LIB)/$(NAME)

document:
	# generate documentation
	mkdir -p $(DOC_DIR)
	$(DOCCER) src/*.lua
	mv ldt/* $(DOC_DIR)
	rm -rf ldt

bundle:
  # tar-ing it (this works only with version 1.19 and beyond, due
  # the --exclude-vcs flag)
	tar --create --verbose --exclude-vcs --file=../$(NAME)-$(VERSION).tar ../$(NAME)
	
  # zipping it
	gzip ../$(NAME)-$(VERSION).tar

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
