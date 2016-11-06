# How to run this lexer

Start the Haskell REPL:

```
stack ghci
```

Load the lexer:

```
:load Lexer.hs
```

Run the tokenizer:
```
tokenizeTest "/**block doc comment*/"
```