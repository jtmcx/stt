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
