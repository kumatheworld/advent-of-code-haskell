#!/bin/bash

if [ $# -eq 0 ]; then
    echo "Usage: $0 <day>"
    echo "Example: $0 1"
    exit 1
fi

DAY=$1
DAY_PADDED=$(printf "%02d" $DAY)
DAY_MODULE="Day${DAY_PADDED}"

# Create module file
MODULE_FILE="src/${DAY_MODULE}.hs"
if [ ! -f "$MODULE_FILE" ]; then
    cat > "$MODULE_FILE" << EOF
module ${DAY_MODULE} (solution) where

import AoC.Template (Day(..), solve)
import qualified Data.Text as T

day :: Day
day = Day $DAY

part1 :: T.Text -> Maybe Int
part1 _input = Nothing

part2 :: T.Text -> Maybe Int
part2 _input = Nothing

solution :: IO ()
solution = solve day part1 part2
EOF
    echo "Created module file \"$MODULE_FILE\""
else
    echo "Module file \"$MODULE_FILE\" already exists"
fi

# Create main file for executable
MAIN_FILE="app/Main${DAY_PADDED}.hs"
if [ ! -f "$MAIN_FILE" ]; then
    cat > "$MAIN_FILE" << EOF
module Main (main) where

import qualified ${DAY_MODULE}

main :: IO ()
main = ${DAY_MODULE}.solution
EOF
    echo "Created main file \"$MAIN_FILE\""
else
    echo "Main file \"$MAIN_FILE\" already exists"
fi

# Create input file
INPUT_FILE="data/inputs/${DAY_PADDED}.txt"
if [ ! -f "$INPUT_FILE" ]; then
    touch "$INPUT_FILE"
    echo "Created empty input file \"$INPUT_FILE\""
else
    echo "Input file \"$INPUT_FILE\" already exists"
fi

# Create example file
EXAMPLE_FILE="data/examples/${DAY_PADDED}.txt"
if [ ! -f "$EXAMPLE_FILE" ]; then
    touch "$EXAMPLE_FILE"
    echo "Created empty example file \"$EXAMPLE_FILE\""
else
    echo "Example file \"$EXAMPLE_FILE\" already exists"
fi

# Update cabal file to add executable and library module
CABAL_FILE="advent-of-code-haskell.cabal"
EXECUTABLE_NAME="day${DAY_PADDED}"

# Add module to library exposed-modules if not already there
if ! grep -q "${DAY_MODULE}" "$CABAL_FILE"; then
    # Find the line with exposed-modules and add the new module
    sed -i '' "/exposed-modules:/a\\
                 , ${DAY_MODULE}
" "$CABAL_FILE"
    echo "Added ${DAY_MODULE} to library exposed-modules"
fi

# Add executable if not already there
if ! grep -q "executable $EXECUTABLE_NAME" "$CABAL_FILE"; then
    cat >> "$CABAL_FILE" << EOF

executable $EXECUTABLE_NAME
  import: shared-properties
  main-is: Main${DAY_PADDED}.hs
  hs-source-dirs: app
  build-depends: base ^>=4.21.0.0
               , advent-of-code-haskell
               , text
EOF
    echo "Added executable \"$EXECUTABLE_NAME\" to cabal file"
fi

echo "---"
echo "🎄 Type \`cabal run $EXECUTABLE_NAME\` to run your solution."
