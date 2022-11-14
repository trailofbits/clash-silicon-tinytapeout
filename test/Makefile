SIM ?= icarus
TOPLEVEL_LANG ?= verilog

SRC = ../src

VERILOG_SOURCES += $(SRC)/tb.v $(SRC)/top.v

# TOPLEVEL is the name of the toplevel module in your Verilog or VHDL file
TOPLEVEL = tb

# MODULE is the basename of the Python test file
MODULE = test

# include cocotb's make rules to take care of the simulator setup
include $(shell cocotb-config --makefiles)/Makefile.sim
