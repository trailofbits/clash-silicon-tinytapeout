![](../../workflows/gds/badge.svg) ![](../../workflows/test/badge.svg)

# clash silicon

a (very small) synthesized cpu written in [clash](https://clash-lang.org/).

# building

```sh
$ nix-shell --pure --run "make"
```

see `default.nix` for dependencies.

# testing

clash-level tests are written in haskell using [quickcheck](https://hackage.haskell.org/package/QuickCheck).
- testing pure haskell functions abstracted away from e.g. clock cycles
- fuzzing

HDL-level tests are written using [cocotb](https://www.cocotb.org/) and a small `src/tb.v` shim.
- verification of the compiled Clash
- clock-cycle dependencies
- GTKWave

```sh
$ nix-shell --pure --run "make lint test"
```

# resources

- [tinytapeout](https://tinytapeout.com/)
- [TT02 verilog template](https://github.com/TinyTapeout/tt02-verilog-demo)
