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

# Constants

Constants variables are inlined in code directly when used:

```Rust
const ANSWER: i32 = 42;
a = ANSWER;
```

```LLVM
@const2751 = internal unnamed_addr constant i32 42, align 4
...
define i32 @main(i32 %argc, i8** %argv) #0 {
  ...
  store i32 42, i32* %a, align 4
  ...
}

```

## Variables

Constant variables are defined as `let a: i32;`.

* Define local integer variable 'a': `%a = alloca i32`
* Set local integer value 26 to variable 'a': `store i32 26, i32* %a, align 4`
* Set global integer variable 'a' with value 7 `@a = global i32 7, align 4`
* Boolen (`true`) variable  `store i8 1, i8* %b, align 1`
* Boolen (`false`) variable `store i8 0, i8* %c, align 1`

Other variable types are defined in similar fashion.

Mutable variables are defined as `let mut a: i32`.


### Constant vs. mutable variables
Constant variables can be assigned with value only once. Both following assignments are allowed.

```Rust
let a: i32;
...
a = 6;
```
```Rust
let a: i32 = 6;
```
Mutable variables can be assigned arbitrarily.

### Note

Rust adds for each unique constant value following statement (example value is 5):
```LLVM
@const2751 = internal unnamed_addr constant i32 5, align 4
```


## Registers

We have infinite number of registers.

* Load integer value from variable 'a' to register 0: `%0 = load i32, i32* %a, align 4`
* Store integer value from register 0 to variable 'a': `store i32 %0, i32* %a, align 4`

* Load boolean value from variable 'a' to register 0:
```LLVM
%0 = load i8, i8* %a, align 1, !range !0`                   ; Can only be 0 or 1
%1 = trunc i8 %0 to i1                  ;truncates its operand to the other type
```
* Store boolean value from register 0 to variable 'a':
```LLVM
%1 = zext i1 %0 to i8                   ; zero extends its operand to other type
store i8 %1, i8* %a, align 1
```

## Arrays

TODO

## Functions

### Function attributes

See [Function Attributes](http://llvm.org/docs/LangRef.html#fnattrs).

```LLVM
attributes #0 = { ... }
```

### Without return value

Function takes one integer parameter 'a' and function attributes #0.

```LLVM
define void @foo(i32 %a) #0 {
  ...
  ret void
}
```

### With return value

Function takes two integer parameters 'a' and 'b' and function attributes #0 and returns value stored in register 6.

```LLVM
define i32 @foo(i32 %a, i32 %b) #0 {
  ...
  ret i32 %6
}
```

### Main function
```LLVM
define i32 @main() #0 {
  ...
  ret i32 0
}
```
or
```LLVM
define i32 @main(i32 %argc, i8** %argv) #0 {
  ...
  ret i32 0
}
```

### Print function

Litle bit of cheating (at least at the beginning) with C-style `printf`. Example `printf("%d\n", a)`:

```LLVM
; Fortmat string
@.str = private unnamed_addr constant [4 x i8] c"%d\0A\00", align 1

define i32 @main() #0 {
  ...
  ; load variable value
  %2 = load i32, i32* %a, align 4
  ; format string lenght = 4
  %3 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([4 x i8], [4 x i8]* @.str, i32 0, i32 0), i32 %2)
  ...
}

declare i32 @printf(i8*, ...) #1

attributes #1 = { "disable-tail-calls"="false" "less-precise-fpmad"="false" "no-frame-pointer-elim"="true" "no-frame-pointer-elim-non-leaf" "no-infs-fp-math"="false" "no-nans-fp-math"="false" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+fxsr,+mmx,+sse,+sse2" "unsafe-fp-math"="false" "use-soft-float"="false" }
```

## Operations

### a = b + c
```LLVM
...
%1 = load i32, i32* %b, align 4
%2 = load i32, i32* %c, align 4
%3 = add nsw i32 %1, %2
store i32 %3, i32* %a, align 4
...
```

### a = b - c
```LLVM
...
%1 = load i32, i32* %b, align 4
%2 = load i32, i32* %c, align 4
%3 = sub nsw i32 %1, %2
store i32 %3, i32* %a, align 4
...
```

### a = b * c
```LLVM
...
%1 = load i32, i32* %b, align 4
%2 = load i32, i32* %c, align 4
%3 = mul nsw i32 %1, %2
store i32 %3, i32* %a, align 4
...
```

### a = b / c
```LLVM
...
%1 = load i32, i32* %b, align 4
%2 = load i32, i32* %c, align 4
%3 = sdiv i32 %1, %2
store i32 %3, i32* %a, align 4
...
```

### a = b % c
```LLVM
...
%1 = load i32, i32* %b, align 4
%2 = load i32, i32* %c, align 4
%3 = srem i32 %1, %2
store i32 %3, i32* %a, align 4
...
```

### Bitwise operations

For boolean typed variables.
```LLVM
%3 = and i1 %1, %2                                                       ; and &
%3 = or i1 %1, %2                                                         ; or |
%3 = xor i1 %1, %2                                                       ; xor ^
%3 = xor i1 %1, true                                                ; negation !
```

For integer typed variables.
```LLVM
%3 = and i32 %1, %2                                                      ; and &
%3 = or i32 %1, %2                                                        ; or |
%3 = xor i32 %1, %2                                                      ; xor ^
%3 = xor i32 %1, -1                                                 ; negation !
```

### Lazy boolean operators

Cab be only applied to operands of boolean type.

#### &&

TODO

#### ||

TODO

## Conditions

TODO

### Operators

TODO

## Loops

### Loop
Simple loop

```LLVM
  ...                                             ; before loop
  br label %loop_body

loop_exit:                                        ; preds = %clean_ast_9_
  ...                                             ; after loop
  ret void                                        ; return something

loop_body:                                        ; preds = %entry-block
  ...                                             ; do something in loop
  br label %loop_body                             ; loop again
  ...
  br label %clean_ast_9_                          ; enough looping (break)

clean_ast_9_:                                     ; preds = %loop_body
  br label %loop_exit                             ; jump to exit after cleanup

```

### While

TODO



## Issues
- How to represent result? As return value? Print?
- How to implement return value (and `exit` function)?
- How to implement `println` function?
