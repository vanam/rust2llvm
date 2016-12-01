# falsum -- Rust to LLVM compiler

This project aims to create educational compiler from [Rust](rust-lang.org) language to [LLVM](http://llvm.org/) in [Haskell](https://www.haskell.org/).

## Dependencies

* [Haskell](https://www.haskell.org/) - `sudo apt-get install haskell-platform`
* [clang](http://clang.llvm.org/) - `sudo apt-get install clang`

## How to compile and run programs in LLVM source inputs

LLVM source files have `*.llvm` file extension. Files can be compiled using *clang*.

```
clang hello.ll -o hello
```

# Development
## Stack Tool

It's a management tool (similar to cabal).
Here is a [guide](https://docs.haskellstack.org/en/stable/README/) for the installation.


## Build & Run

How to build our `falsum` app:

```
stack build
```

How to run it:

```
stack exec falsum
```

## Tests

Tests are executed by:

```
stack test
```

## Source code formatting

You have to install `hfmt`:

```
stack install hfmt
```

and run the formatting itself:

```
stack exec hfmt -- -w
```

---

# LLVM

## Variables

* Define local integer variable 'a': `%a = alloca i32`
* Set local integer value 26 to variable 'a': `store i32 26, i32* %a, align 4`
* Set global integer variable 'a' with value 7 `@a = global i32 7, align 4`

Other variable types are defined in similar fashion.

## Registers

We have infinite number of registers.

* Load integer value from variable 'a' to register 0: `%0 = load i32, i32* %a, align 4`
* Store integer value from register 0 to variable 'a': `store i32 %0, i32* %a, align 4`

## Functions

### Function attributes

See [Function Attributes](http://llvm.org/docs/LangRef.html#fnattrs).

`attributes #0 = { ... }`

### Without return value

Function takes one integer parameter 'a' and function attributes #0.

```
define void @foo(i32 %a) #0 {
  ...
  ret void
}
```

### With return value

Function takes two integer parameters 'a' and 'b' and function attributes #0 and returns value stored in register 6.

```
define i32 @foo(i32 %a, i32 %b) #0 {
  ...
  ret i32 %6
}
```

### Main function
```
define i32 @main() #0 {
  ...
  ret i32 0
}
```
or
```
define i32 @main(i32 %argc, i8** %argv) #0 {
  ...
  ret i32 0
}
```
