CLASH_SRC = $(wildcard src/*.hs)
TOP_MODULE = jleightcap_top
TOP = src/$(TOP_MODULE).v

GHC_FLAGS += -isrc -Wall
CLASH_FLAGS += $(GHC_FLAGS) -fclash-clear -fclash-error-extra -fclash-compile-ultra -fclash-aggressive-x-optimization
VERILATOR_FLAGS += -Wall -Wno-WIDTH

all: $(TOP)

$(TOP): $(CLASH_SRC)
	clash $(CLASH_FLAGS) src/Top.hs --verilog
	cp verilog/Top.top/$(TOP_MODULE).v $@

.PHONY: lint
lint: $(TOP)
	verilator --lint-only $(VERILATOR_FLAGS) $(TOP)
	hlint $(CLASH_SRC)

.PHONY: test
test:
	runghc $(GHC_FLAGS) src/Test.hs

.PHONY: clean
clean:
	rm -rf verilog/ src/*.hi src/*.o
