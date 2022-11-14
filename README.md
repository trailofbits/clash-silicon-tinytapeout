![](../../workflows/gds/badge.svg) ![](../../workflows/test/badge.svg)

# clash silicon

a (very small) synthesized cpu written in [clash](https://clash-lang.org/).

[interactive viewer](https://jleightcap.github.io/clash-silicon-tinytapeout/)

# cpu description

a small minsky-like register machine consisting of

- register file (`rf`): 8× 5-bit register
- register file pointer (`ptr`): 3-bit pointer into register file
- cjump: 1-bit conditional branch taken

the register `R` is the contents of the `rf[ptr]`.

| encoding | meaning |
| -------- | ------- |
| `0_nnnnn` | `R` ← 5-bit literal `n` |
| `100_ppp` | `ptr` ← 3-bit literal `p` |
| `101_ppp` | if `R == 0` then `ptr` ← 3-bit literal `p` else NOP |
| `1100_nn` | `R` ← `R` + 2-bit literal `n` |
| `1101_nn` | `R` ← `R` - 2-bit literal `n` |
| ... | ... |

# building

```sh
$ nix-shell --pure --run "make"
```

see `default.nix` for dependencies.

# testing

```sh
$ nix-shell --pure --run "make lint test"
```

## clash
clash-level tests are written in haskell using [quickcheck](https://hackage.haskell.org/package/QuickCheck).
- testing pure haskell functions abstracted away from e.g. clock cycles
- fuzzing

## HDL
HDL-level tests are written using [cocotb](https://www.cocotb.org/) and a small `src/tb.v` shim.
- verification of the compiled Clash
- clock-cycle dependencies
- GTKWave

to generate GTKWave output for a single cocotb test, here `test_regfile`,

```sh
$ TESTCASE="test_regfile" nix-shell --run "make -C test"
```

# special thanks

- [Matt Venn](https://mattvenn.net/)
- Connor Northway & Eddie Zhou (feedback and rubber ducking)

# resources

- [tinytapeout](https://tinytapeout.com/)
- [TT02 verilog template](https://github.com/TinyTapeout/tt02-verilog-demo)
