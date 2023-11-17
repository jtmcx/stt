alias b := build
alias t := test

# Print 'just -l'
help:
	@just -l

# Build the 'stt' executable
build:
	cabal build

# Build and run the 'stt' executable
run:
	cabal run

# Run the test suite
test:
	cabal test --test-show-details=streaming

# Generate local haddoc docs (with source code included)
doc:
	cabal haddock --haddock-hyperlinked-source

# Run 'cabal clean'
clean:
	cabal clean

# Drop into ghci with 'stt' loaded
ghci:
	cabal repl

# Drop into ghci with 'stt-test' loaded
ghci-test:
	cabal repl stt-test

# -----------------------------------------------------------------------------
# HACK: There seems to be some whackiness with GHC and unicode on my machine.
# When trying to print a unicode character, I get errors that look like the
# following:
#
#    <stdout>: commitBuffer: invalid argument (invalid character)
#    <stdout>: hPutChar: invalid argument (invalid character)
#
# I haven't fully worked out the cause, but I gleaned from this issue[1] that
# it's a locale problem. Even though my locale is set to "en_US.UTF-8", setting
# "LANG=C.UTF-8" seems to work. I think this is because of how ghc was built
# upstream in Nix. Hopefully I'll figure out this issue and this hack can go
# away. ¯\_(ツ)_/¯
#
# [1]: https://github.com/commercialhaskell/stack/pull/4294

export LANG := "C.UTF-8"
