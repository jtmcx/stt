# Set Theoretic Types

This repository houses *stt*, a toy language for exploring set theoretic types.

```
-- Ye olde factorial example.
sig fact' : Int → Int
def fact' = fix λfact' x,
  case x of
  | 0   ⇒ 1
  | Int ⇒ x * fact' (x - 1)

-- A more "dynamic" version that accepts an 'x' of any type.
sig fact : Any → (Int | false)
def fact = λx,
  case x of
  | Int ⇒ (case x >= 0 of
           | true  ⇒ fact' x
           | false ⇒ false)
  | Any ⇒ false
```

This project implements the type system described in
[Covariance and Controvariance: a fresh look at an old
issue](https://arxiv.org/abs/1809.01427). For more information, check
out the incredible tutorial, [Down and Dirty with Semantic Set-theoretic
Types](https://pnwamk.github.io/sst-tutorial/), by Andrew M. Kent.

The *stt* language is small enough to be accessible, but just large
enough to allow for the creation of non-trivial programs. There are
examples that demonstrate how to use [recursion](examples/recursion.stt)
and [lists](examples/lists.stt). Check out the [examples](examples)
directory for more.

## Status

The type-checker is *almost* complete; it requires a bit more glue before
it can be enabled. Runtime type checking mostly works (a la the `case`
statement), but static types (i.e. annotations and type signatures)
are ignored.

The evaluator is fully functional. There is even a (crude) single-step
debugger avilable in the REPL. The pretty-printer was hacked on a little
bit too much, and has some warts. It generally prints the right thing.

## Running

The easiest way to build is with Nix. I personally use `direnv` to
automatically drop into a `nix shell`:

```
$ echo "use flake" > .envrc
$ direnv allow
```

This project uses [`just`](https://github.com/casey/just), which is
just (ha) a thin wrapper around `cabal`:

```
$ just
Available recipes:
    build     # Build the 'stt' executable
    b         # alias for `build`
    clean     # Run 'cabal clean'
    doc       # Generate local haddoc docs (with source code included)
    ghci      # Drop into ghci with 'stt' loaded
    ghci-test # Drop into ghci with 'stt-test' loaded
    help      # Print 'just -l'
    run       # Build and run the 'stt' executable
    test      # Run the test suite
    t         # alias for `test`
```

To drop into an *stt* REPL, use `just run`:

```
$ just run
stt> :help
:ast             Parse an expression and print its AST
:env             Print the current environment
:h :help         Print this message
:l :load         Load definitions from a file
:paste           Read multiple lines of input
:pp :pretty      Pretty print an expression without evaluating it
:reset           Reset the REPL state to an empty environment
:debug           Print all steps of an evaulation
:q :quit         Exit the REPL
:unicode [y|n]   Enable/disable unicode output
```

To run the factorial example above:

```
stt> :load examples/recursion.stt
stt> fact 6
720
stt> fact true
false
```
