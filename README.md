# 🎄 Advent of Code {year}

Solutions for [Advent of Code](https://adventofcode.com/) in [Haskell](https://www.haskell.org/).

> **Note**: This template is inspired by and adapted from [fspoettel/advent-of-code-rust](https://github.com/fspoettel/advent-of-code-rust). Many thanks to Felix Spoettel for the excellent original Rust template!
> 
> Haskell adaptation created with assistance from Kiro CLI, GitHub Copilot, and Claude Code.

<!--- advent_readme_stars table --->

<!--- benchmarking table --->

## Template setup

This template supports all major OS (macOS, Linux, Windows).

### 📝 Create your repository

1. Clone this repository to your computer.
2. If you are solving a previous year's advent of code, change the year in `.aoc-year`.

### 💻 Setup Haskell

1. Install [GHCup](https://www.haskell.org/ghcup/) (recommended) or the [Haskell Platform](https://www.haskell.org/platform/).
2. Install GHC and Cabal: `ghcup install ghc && ghcup install cabal`
3. (recommended) Install the [Haskell Language Server](https://haskell-language-server.readthedocs.io/): `ghcup install hls`
4. (optional) Install a native debugger. If you are using VS Code, the [Haskell extension](https://marketplace.visualstudio.com/items?itemName=haskell.haskell) provides good integration.

### 🔑 Setup session cookie

To automatically download puzzle inputs, you need your Advent of Code session cookie:

1. Log in to [Advent of Code](https://adventofcode.com/)
2. Open browser DevTools (F12) → Application/Storage → Cookies
3. Copy the value of the `session` cookie
4. Save it to `session.cookie` in the project root (this file is gitignored)

✨ You can start solving puzzles now! Head to the [Usage section](#usage) to see how to use this template.

## Development container (recommended)

This repository includes a VS Code devcontainer configuration that sets up a Haskell development environment automatically.

- What it installs: `ghcup`, GHC, `cabal`, Haskell Language Server (HLS), and `ormolu` (the Haskell formatter).
- How it runs: when you reopen the project in a Codespace or "Remote - Containers" session, the container runs `.devcontainer/postCreate.sh` which installs the tools. This may take a few minutes on first run.
- Editor integration: the repository also contains workspace settings and extension recommendations to enable format-on-save for Haskell files using Ormolu (VS Code will recommend the `haskell.haskell` extension).

Usage:

1. In VS Code open the Command Palette and choose **Remote-Containers: Reopen in Container** (or open the repo in Codespaces). The container will be provisioned and the post-create script will run.
2. After the container finishes, the Haskell extension should activate. Open a `.hs` file and try **Format Document** (Shift+Alt+F) to verify Ormolu is working. Format-on-save will then run automatically.

Troubleshooting:

- If formatting doesn't run, check the Output panel → `Haskell` / `HLS` for errors.
- If the workspace is untrusted in VS Code, trust it to allow workspace settings and recommended extensions.
- The post-create script installs tools with `ghcup` and `cabal`; if you prefer a prebuilt image for faster starts, ask and I can add a Dockerfile-based devcontainer image.


## Usage

### ➡️ Scaffold a new day

```sh
# Creates files and downloads input automatically
cabal run scaffold 1

# output:
# Created module file "src/Day01.hs"
# Created empty file "data/examples/01-1.txt"
# Cabal file already up to date
# Spec already up to date
# Added Day01 to day runner

#+ Downloading input...
# Downloaded input for day 1 to data/inputs/01.txt
# ---
# 🎄 Type `cabal run day 1` to run your solution.
#    Add your example inputs to data/examples/01-1.txt, 01-2.txt, etc.
```

This creates:
- Solution module in `src/Day01.hs`
- Empty example file in `data/examples/01-1.txt`
- Downloads real input to `data/inputs/01.txt`
- Updates `advent-of-code-haskell.cabal` to expose the module
- Auto-registers the new day's tests in `test/Spec.hs` so `cabal test` runs them
- Registers the new day in `app/Day.hs` so you can run the solution with `cabal run day <n>`

### ➡️ Run solutions for a day

```sh
cabal run day 1

# output:
# Part 1: Just 42
# Part 2: Just 42
```

### ➡️ Submit solutions

```sh
# Automatically compute and submit
cabal run submit 1 1  # day 1, part 1
cabal run submit 1 2  # day 1, part 2

# Or provide answer directly
cabal run submit 1 1 42  # day 1, part 1, answer 42
```

When you run `cabal run submit <day> <part>` without an answer, it will:
1. Read the input file for that day
2. Run your solution (part1 or part2 function)
3. Submit the computed answer to Advent of Code

The scaffold automatically registers each day's solution with the submit command.

### ➡️ Run all solutions

```sh
cabal run advent-of-code

# output:
# ----------
# | Day 01 |
# ----------
# Part 1: Just 42
# Part 2: Just 42
# <...other days...>
```

### ➡️ Run tests

```sh
# Run all tests
cabal test

# Run tests for a specific day
cabal run test 1
```

Tests are defined in each day's module (e.g., `src/Day01.hs`). The template supports multiple example files:
- `data/examples/01-1.txt` - Example 1 (read with `readExample day 1`)
- `data/examples/01-2.txt` - Example 2 (read with `readExample day 2`)
- `data/examples/01-3.txt` - Example 3 (read with `readExample day 3`)

This is useful when a puzzle has multiple examples or different test cases. Use `cabal test --test-show-details=direct` for verbose output.

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
│   ├── Main.hs                  # Main executable that runs all solutions
│   ├── Day.hs                   # Day runner (cabal run day <n>)
│   ├── Download.hs              # Download puzzle inputs
│   ├── Scaffold.hs              # Scaffold new day files
│   ├── Submit.hs                # Submit solutions
│   └── (scaffold auto-updates Day.hs to register new days)
│   ├── inputs/
│   │   ├── 01.txt               # Day 1 input
│   │   └── ...
│   └── examples/
│       ├── 01.txt               # Day 1 example
│       └── ...
├── test/
│   └── Spec.hs                  # Test suite
├── .aoc-year                    # Year configuration (change for different years)
└── advent-of-code-haskell.cabal # Project configuration
```

## Useful Libraries

- [containers](https://hackage.haskell.org/package/containers): Efficient container types (Map, Set, etc.)
- [vector](https://hackage.haskell.org/package/vector): Efficient arrays
- [text](https://hackage.haskell.org/package/text): Efficient Unicode text processing
- [megaparsec](https://hackage.haskell.org/package/megaparsec): Parser combinators
- [linear](https://hackage.haskell.org/package/linear): Linear algebra for 2D/3D problems

A curated list of popular Haskell packages can be found on [Hackage](https://hackage.haskell.org/).
