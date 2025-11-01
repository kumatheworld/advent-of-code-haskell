# 🎄 Advent of Code {year}

Solutions for [Advent of Code](https://adventofcode.com/) in [Haskell](https://www.haskell.org/).

<!--- advent_readme_stars table --->\n\n<!--- benchmarking table --->\n\n---

## Template setup

This template supports all major OS (macOS, Linux, Windows).

### 📝 Create your repository

1. Clone this repository to your computer.
2. If you are solving a previous year's advent of code, change the year in the README and solution files.

### 💻 Setup Haskell

1. Install [GHCup](https://www.haskell.org/ghcup/) (recommended) or the [Haskell Platform](https://www.haskell.org/platform/).
2. Install GHC and Cabal: `ghcup install ghc && ghcup install cabal`
3. (recommended) Install the [Haskell Language Server](https://haskell-language-server.readthedocs.io/): `ghcup install hls`
4. (optional) Install a native debugger. If you are using VS Code, the [Haskell extension](https://marketplace.visualstudio.com/items?itemName=haskell.haskell) provides good integration.

---

✨ You can start solving puzzles now! Head to the [Usage section](#usage) to see how to use this template.

## Usage

### ➡️ Scaffold a day

```sh
# example: `./scripts/scaffold.sh 1`
./scripts/scaffold.sh <day>

# output:
# Created module file "src/Day01.hs"
# Created empty input file "data/inputs/01.txt"
# Created empty example file "data/examples/01.txt"
# ---
# 🎄 Type `cabal run day01` to run your solution.
```

Individual solutions live in the `./src/` directory as separate modules. _Inputs_ and _examples_ live in the `./data` directory.

Every solution has _tests_ referencing its _example_ file in `./data/examples`. Use these tests to develop and debug your solutions against the example input.

### ➡️ Run solutions for a day

```sh
# example: `cabal run day01`
cabal run day<day>

# output:
# Part 1: 42
# Part 2: 42
```

The solution executables run your solution against real puzzle inputs.

### ➡️ Run all solutions

```sh
cabal run advent-of-code

# output:
# ----------
# | Day 01 |
# ----------
# Part 1: 42
# Part 2: 42
# <...other days...>
```

This runs all solutions sequentially and prints output to the command-line.

### ➡️ Run tests

```sh
cabal test
```

To run tests for a specific day, you can use `cabal test --test-show-details=direct` for more verbose output.

### ➡️ Build optimized solutions

```sh
cabal build --enable-optimization=2
```

### ➡️ Format code

```sh
# Install ormolu first: cabal install ormolu
find src -name "*.hs" -exec ormolu --mode inplace {} \;
```

## Project Structure

```
advent-of-code-haskell/
├── src/
│   ├── AoC/
│   │   └── Template.hs          # Shared utilities and template functions
│   ├── Day01.hs                 # Day 1 solution
│   ├── Day02.hs                 # Day 2 solution
│   └── ...
├── app/
│   └── Main.hs                  # Main executable that runs all solutions
├── data/
│   ├── inputs/
│   │   ├── 01.txt               # Day 1 input
│   │   └── ...
│   └── examples/
│       ├── 01.txt               # Day 1 example
│       └── ...
├── test/
│   └── Spec.hs                  # Test suite
├── scripts/
│   └── scaffold.sh              # Script to scaffold new days
└── advent-of-code-haskell.cabal # Project configuration
```

## Useful Libraries

- [containers](https://hackage.haskell.org/package/containers): Efficient container types (Map, Set, etc.)
- [vector](https://hackage.haskell.org/package/vector): Efficient arrays
- [text](https://hackage.haskell.org/package/text): Efficient Unicode text processing
- [megaparsec](https://hackage.haskell.org/package/megaparsec): Parser combinators
- [linear](https://hackage.haskell.org/package/linear): Linear algebra for 2D/3D problems

A curated list of popular Haskell packages can be found on [Hackage](https://hackage.haskell.org/).
