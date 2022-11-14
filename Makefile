TOP_MODULE = top
TOP = src/$(TOP_MODULE).v

CLASH_SRC = $(wildcard src/*.hs)
CLASH_FLAGS += $(GHC_FLAGS) -fclash-clear -fclash-error-extra -fclash-compile-ultra -fclash-aggressive-x-optimization
GHC_FLAGS += -isrc -Wall

# -Wno-WIDTH: 			clash-generates some verilog that doesen't pass this lint.
# -Wno-MULTITOP:		just linting all sources; don't care about multiple top-level modules.
VERILATOR_FLAGS += -Wall -Wno-WIDTH -Wno-MULTITOP

.PHONY: all
all: $(TOP)

$(TOP): $(CLASH_SRC)
	clash $(CLASH_FLAGS) src/Top.hs --verilog
	sed '/timescale/d' verilog/Top.topEntity/$(TOP_MODULE).v > $@

.PHONY: lint
lint: $(TOP)
	verilator --lint-only $(VERILATOR_FLAGS) $(wildcard src/*.v)
	hlint $(CLASH_SRC)

.PHONY: test
test: $(TOP)
	runghc $(GHC_FLAGS) src/Test.hs
	$(MAKE) -C test/ && ! grep failure test/results.xml

.PHONY: clean
clean:
	$(MAKE) -C test/ clean
	rm -rf verilog/ src/*.hi src/*.o \
		test/__pycache__ test/results.xml
