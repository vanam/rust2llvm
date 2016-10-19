# Rust to LLVM compiler

This project aims to create educational compiler from [Rust](rust-lang.org) language to [LLVM](http://llvm.org/) in [Haskell](https://www.haskell.org/).

## Dependencies

* [Haskell](https://www.haskell.org/) - `sudo apt-get install haskell-platform`
* [clang](http://clang.llvm.org/) - `sudo apt-get install clang`

## How to compile and run programs in LLVM source inputs

LLVM source files have `*.llvm` file extension. Files can be compiled using *clang*.

```
clang hello.ll -o hello
```
