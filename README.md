# Battleship Logic Guesser (Haskell)

This repository contains a Haskell module that implements a logical guessing game inspired by Battleships.
The game is played on a 4-row by 8-column grid and involves deducing the locations of three hidden ships using minimal guesses.
This was written in 2021 for an assignment for a declarative programming subject.

## Game Overview

- **Grid Size:** 4 rows × 8 columns
- **Ships:** 3 hidden ships at distinct locations
- **Guesses:** 3 distinct locations per turn
- **Feedback:** Number of guessed locations that are Chebyshev distance 0, 1, and 2 from the targets

Using feedback from previous guesses, the algorithm filters out inconsistent targets and selects the guess that minimises the expected number of remaining valid target combinations.

## How It Works
1.	initialGuess makes a strategic first guess and prepares all possible targets.
2.	feedback compares a guess to the hidden target and gives proximity information.
3.	nextGuess filters out inconsistent targets and chooses the most informative next guess.

## Example Usage

```haskell
import Guesser

-- Suppose the actual hidden target is:
let actualTarget = [Location 2 1, Location 4 2, Location 7 4]

-- Get the initial guess and game state
let (guess1, state1) = initialGuess

-- Get feedback from the actual target
let fb1 = feedback actualTarget guess1

-- Get the next guess based on feedback
let (guess2, state2) = nextGuess (guess1, state1) fb1

-- Repeat: get feedback for new guess
let fb2 = feedback actualTarget guess2

-- Get the third guess
let (guess3, state3) = nextGuess (guess2, state2) fb2
```

