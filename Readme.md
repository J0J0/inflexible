# inflexible – A package to reason about semi-free differential graded-commutative algebras.

## Build instructions
Currently, the package relies on patched versions of some upstream libraries.
The `cabal.project` file contains the necessary git sources, so a simple

    cabal build

should work (provided your cabal version is not ancient).

## Documentation
Similarily,

    cabal haddock

will generate the API documentation of the package in HTML format.


## Examples
Lots of examples and tools are provided in `interactive/`.
To run the examples, use the bash scripts
`interactive/ghci.sh` (no Sage initialization) or
`interactive/sageenvghci.sh` (with Sage initialization). 
Except for longer startup time, there is no drawback in
always using the latter.

For example,

    ./interactive/sageenvghci.sh interactive/ex-A1-crowley-loeh.hs

opens a GHCi session and load the specified file.

## Without Python/Sage
In case the Sage integration does not work for you, the branch
`no-sage` contains a version of the package where the Sage parts
have been removed.
