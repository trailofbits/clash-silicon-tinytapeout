![](../../workflows/gds/badge.svg) ![](../../workflows/test/badge.svg)

# clash silicon

a (very small) synthesized cpu written in [clash](https://clash-lang.org/).

# building

```sh
$ nix-shell --pure --run "make"
```

see `default.nix` for dependencies.

# testing

rather than HDL-level testing (e.g. [cocotb](https://www.cocotb.org/)), tests are written in pure haskell, which is then lowered to HDL.

```sh
$ nix-shell --pure --run "make lint test"
```

# resources

- [tinytapeout](https://tinytapeout.com/)
- [TT02 verilog template](https://github.com/TinyTapeout/tt02-verilog-demo)
