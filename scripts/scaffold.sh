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
module ${DAY_MODULE} (solve) where

import AoC.Template (Day(..), solve)
import qualified Data.Text as T

day :: Day
day = Day $DAY

part1 :: T.Text -> Maybe Int
part1 input = Nothing

part2 :: T.Text -> Maybe Int
part2 input = Nothing

solve :: IO ()
solve = AoC.Template.solve day part1 part2
EOF
    echo "Created module file \"$MODULE_FILE\""
else
    echo "Module file \"$MODULE_FILE\" already exists"
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

# Update cabal file to add executable
CABAL_FILE="advent-of-code-haskell.cabal"
EXECUTABLE_NAME="day${DAY_PADDED}"

if ! grep -q "executable $EXECUTABLE_NAME" "$CABAL_FILE"; then
    cat >> "$CABAL_FILE" << EOF

executable $EXECUTABLE_NAME
  import: shared-properties
  main-is: ${DAY_MODULE}.hs
  hs-source-dirs: src
  build-depends: base ^>=4.17.0.0
               , advent-of-code-haskell
               , text
  other-modules: AoC.Template
EOF
    echo "Added executable \"$EXECUTABLE_NAME\" to cabal file"
fi

echo "---"
echo "🎄 Type \`cabal run $EXECUTABLE_NAME\` to run your solution."
