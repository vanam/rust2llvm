# Stack Tool

It's a management tool (similar to cabal).
Here is a [guide](https://docs.haskellstack.org/en/stable/README/) for the installation.


# Build & Run

How to build our `falsum` app:

```
stack build
```

How to run it:

```
stack exec falsum
```

# Tests

Tests are executed by:

```
stack test
```

# Source code formatting

You have to install `hfmt`:

```
stack install hfmt
```

and run the formatting itself:

```
stack exec hfmt -- -w
```