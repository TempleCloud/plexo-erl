#------------------------------------------------------------------------------
# PlexoServ Makefile
#------------------------------------------------------------------------------

# The erl binary path for application dependencies.
#
PRJ_DEP_PATH= \
	-pa './deps/jsx/ebin' \
	-pa './deps/jsxn/ebin' \
	-pa './deps/cowlib/ebin' \
	-pa './deps/ranch/ebin' \
	-pa './deps/cowboy/ebin'


# The erl binary path for applications.
#
PRJ_APP_PATH= \
	-pa '../plexo_srv/ebin'


#------------------------------------------------------------------------------
# Default
#------------------------------------------------------------------------------

# Build and run the PlexoSrv.
#
all: build


#------------------------------------------------------------------------------
# Run Commands
#------------------------------------------------------------------------------

# Build and run the PlexoSrv.
#
run:
	@erl \
	  $(PRJ_DEP_PATH) \
	  $(PRJ_APP_PATH) \
	  -s plexo_srv start


# Build and run the PlexoSrv with a connected shell.
#
run-noshell:
	@erl \
	  -noshell \
	  $(PRJ_DEP_PATH) \
	  $(PRJ_APP_PATH) \
	  -s plexo_srv start


# Build and run the TypEr tool. Output to 'TypEr.nfo'.
#
run-typer:
	@typer ./src/*.erl > types.nfo
	@echo "Wrote 'types.nfo'."


# Build the Dialyzer persistent lookup table. Required for the dialyzer tasks.
#
run-build-dialyzer-plt: clean get-deps compile
	@dialyzer --build_plt \
	  --apps erts kernel stdlib crypto ssl \
	  -r .
	@echo "Built Dialyzer PLT."


# Run the dialyzer tool.
#
run-dialyzer:
	@dialyzer --src ./src $(PRJ_DEP_PATH)


# Run the eunit tests.
#
run-doc:
	@rebar doc


# Run the eunit tests.
#
run-eunit:
	@rebar eunit


#------------------------------------------------------------------------------
# Rebar Build Tasks
#------------------------------------------------------------------------------

# Do rebar clean project.
#
clean:
	@rebar clean


# Do rebar get project dependencies.
#
get-deps:
	test -d deps || @rebar get-deps


# Do rebar compile.
#
compile: clean get-deps
	@rebar compile


# Do run rebar success type checker.
#
type-check: compile
	@dialyzer --src ./src


# Do rebar run tests.
#
test: type-check
	@rebar eunit
	# rebar ct -- disabled.
	# rebar qc -- disabled.


# Do rebar generate edoc.
#
# NB: Types must be checked first.
#
doc: type-check
	@rebar doc


# Do clean, deps, compile, check, doc.
#
build: clean get-deps compile type-check test doc


#------------------------------------------------------------------------------
# PHONY
#------------------------------------------------------------------------------


# Do not check the 'test' and 'doc' directories for changes when running the
# the associated tasks.
#
.PHONY: test doc


ifeq ("x","y")
all: clean
	test -d deps || rebar get-deps
	rebar compile
	@erl \
	  -noshell \
	  -pa './deps/jsx/ebin' \
	  -pa './deps/jsxn/ebin' \
	  -pa './deps/cowlib/ebin' \
	  -pa './deps/ranch/ebin' \
	  -pa './deps/cowboy/ebin' \
	  -pa '../plexo_srv/ebin' \
	  -s plexo_srv start
endif