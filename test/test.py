import random

import cocotb
from cocotb.clock import Clock
from cocotb.triggers import ClockCycles, FallingEdge, RisingEdge, Timer


def LIT_INSTR(n):
    assert n <= 0b11111
    return 0b0_00000 | n


def PTR_INSTR(p):
    assert p <= 0b111
    return 0b100_000 | p


def CJUMP_ZE_INSTR(p):
    assert p <= 0b111
    return 0b101_000 | p


def CJUMP_EQ_INSTR(p):
    assert p <= 0b111
    return 0b110_000 | p


def ADD_INSTR(n):
    assert n <= 0b11
    return 0b1110_00 | n


def SUB_INSTR(n):
    assert n <= 0b11
    return 0b111100 | n


async def init(dut):
    clock = Clock(dut.clk, period=2, units="us")
    cocotb.start_soon(clock.start())

    dut._log.info("reset")
    dut.rst.value = 1
    await ClockCycles(dut.clk, 10)
    dut.rst.value = 0


def rand_lit():
    return random.randint(0, 0b011111)


def rand_lit_gen(n=20):
    for LIT in range(n):
        yield rand_lit()


@cocotb.test()
async def test_reset(dut):
    await init(dut)

    # insert non-zero literal
    await FallingEdge(dut.clk)
    nz = 4
    dut.instr.value = LIT_INSTR(nz)
    await FallingEdge(dut.clk)
    assert int(dut.r.value) == nz and nz != 0

    # reset
    dut.rst.value = 1
    await ClockCycles(dut.clk, 10)
    assert int(dut.r.value) == 0, f"not cleared {int(dut.r.value)}"


@cocotb.test()
async def test_literal(dut):
    await init(dut)

    for LIT in rand_lit_gen():
        dut.instr.value = LIT
        await ClockCycles(dut.clk, 2)
        assert (
            int(dut.r.value) == LIT
        ), f"literal failed with {LIT} != {int(dut.r.value)}"


@cocotb.test()
async def test_add(dut):
    await init(dut)

    for ADD_LIT in range(4):
        for LIT in rand_lit_gen():
            dut.instr.value = LIT
            await ClockCycles(dut.clk, 2)  # R <- LIT
            assert int(dut.r.value) == LIT
            dut.instr.value = ADD_INSTR(ADD_LIT)
            await ClockCycles(dut.clk, 2)  # R <- R + ADD_LIT
            assert (
                int(dut.r.value) == (LIT + ADD_LIT) % 32
            ), f"add failed with {LIT} + {ADD_LIT} != {int(dut.r.value)}"


@cocotb.test()
async def test_add_iter(dut):
    await init(dut)

    # test incrementing by two repeatedly
    await FallingEdge(dut.clk)
    dut.instr.value = ADD_INSTR(2)
    n = int(dut.r.value)
    await RisingEdge(dut.clk)
    iters = 100
    await ClockCycles(dut.clk, iters)
    assert (
        int(dut.r.value) == (n + 2 * iters) % 32
    ), f"add100: {n} -/-> {int(dut.r.value)}"


@cocotb.test()
async def test_add_zero(dut):
    await init(dut)

    # test incrementing by zero repeatedly
    await FallingEdge(dut.clk)
    dut.instr.value = ADD_INSTR(0)
    n = int(dut.r.value)
    await RisingEdge(dut.clk)
    await ClockCycles(dut.clk, 100)
    assert int(dut.r.value) == n, f"add0: {n} != {int(dut.r.value)}"


@cocotb.test()
async def test_sub_iter(dut):
    await init(dut)

    # test incrementing by two repeatedly
    await FallingEdge(dut.clk)
    dut.instr.value = SUB_INSTR(2)
    n = int(dut.r.value)
    await RisingEdge(dut.clk)
    iters = 100
    await ClockCycles(dut.clk, iters)
    assert (
        int(dut.r.value) == (n - 2 * iters) % 32
    ), f"sub100: {n} -/-> {int(dut.r.value)}"


@cocotb.test()
async def test_regfile_init(dut):
    await init(dut)

    # assert memory is zeroed initially
    for p in range(16):
        await FallingEdge(dut.clk)
        dut.instr.value = PTR_INSTR(p % 8)
        await ClockCycles(dut.clk, 1)
        assert int(dut.r.value) == 0, f"rf[{p}] = {int(dut.r.value)}"


@cocotb.test()
async def test_regfile_state(dut):
    await init(dut)

    #
    # insert a random list into register file and read it back
    #

    # insert
    rs = list(rand_lit_gen(8))
    for (i, p) in zip(range(8), rs):
        # set pointer to r[i]
        await FallingEdge(dut.clk)
        dut.instr.value = PTR_INSTR(i)
        await FallingEdge(dut.clk)
        # insert literal p
        await FallingEdge(dut.clk)
        dut.instr.value = LIT_INSTR(p)
        await FallingEdge(dut.clk)
        assert int(dut.r.value) == p, f"set R[{i}] = {int(dut.r.value)}, not {p}"
    dut._log.info(f"wrote {list(rs)}")

    # read back
    for (i, p) in zip(range(8), rs):
        # set pointer to r[i]
        await FallingEdge(dut.clk)
        dut.instr.value = PTR_INSTR(i)
        await FallingEdge(dut.clk)
        assert int(dut.r.value) == p, f"read R[{i}] = {int(dut.r.value)}, not {p}"


@cocotb.test()
async def test_cjump_t(dut):
    await init(dut)

    # insert literal p
    await FallingEdge(dut.clk)
    p = rand_lit()
    dut.instr.value = LIT_INSTR(p)
    await FallingEdge(dut.clk)

    # set pointer to r[1]
    dut.instr.value = PTR_INSTR(1)
    await FallingEdge(dut.clk)
    assert int(dut.r.value) == 0

    # r[0] == 0?
    dut.instr.value = CJUMP_ZE_INSTR(0)
    await FallingEdge(dut.clk)
    assert (
        int(dut.r.value) == p
    ), f"cjump target unset: r[0] = {int(dut.r.value)} != {p}"
    assert dut.cjump.value, "cjump unset when zero"

    dut._log.info(f"r[1]=0, jumped to {p}")


@cocotb.test()
async def test_cjump_f(dut):
    await init(dut)

    # insert literal p
    await FallingEdge(dut.clk)
    p = rand_lit()
    dut.instr.value = LIT_INSTR(p)
    await FallingEdge(dut.clk)

    # set pointer to r[1]
    dut.instr.value = PTR_INSTR(1)
    await FallingEdge(dut.clk)

    # insert non-zero literal
    nz = 4
    dut.instr.value = LIT_INSTR(nz)
    await FallingEdge(dut.clk)
    assert int(dut.r.value) == nz and nz != 0

    dut.instr.value = CJUMP_ZE_INSTR(0)
    await FallingEdge(dut.clk)
    assert not dut.cjump.value, "cjump set when nonzero"
    assert int(dut.r.value) == nz, f"cjump target set: r[0] = {int(dut.r.value)} != {p}"

    dut._log.info(f"r[1]={nz} nonzero")
