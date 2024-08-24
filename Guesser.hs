-- Purpose: This file provides functions and types to guess and give feedback 
-- on the locations of ships in a logical guessing game similar to Battleships 
-- 
-- | In this logical guessing game the functions provided in this file provide 
-- the ability to play as both the hider in giving the feedback on how close
-- the guess is to the actual locations of the target, and as the searcher who
-- is deciding which guesses to make based on the information provided in the
-- feedback. It is played on a 4 row, 8 column grid, and there are 3 ships
-- hidden in distinct locations, and guesses consist of 3 distinct locations
-- also. The feedback consists of the number of locations in the guess which
-- were of distance 0, 1 and 2 away from a ship, using the Chebyshev distance
-- metric. Using this feedback, the searcher can narrow down the list of
-- potential targets by only considering guesses that are consistent with 
-- every previous guess's feedback. With the aim to find the ships in as few
-- guesses as possible, the guess chosen is the one which minimises the
-- expected number of remaining targets.

module Guesser (Location, toLocation, fromLocation, feedback,
                GameState, initialGuess, nextGuess) where

import Data.Char
import Data.List

-- | The 'Location' type consists of 'Column' and 'Row', both of which are
-- Ints and represent the row from 1..4 and the column 1..8 (represented by the
-- letters 'A'..'H' in the original String of the location)
type Column = Int
type Row = Int
data Location = Location Column Row
  deriving (Eq, Show, Ord)

-- | The 'GameState' is equivalent to a list of lists of 'Location's, and is 
-- also equivalent to a list of 'Guess'/'Target' type elements. 'Guess' may be
-- used to represent a possible guess or a final guess, and 'Target' (which is
-- equivalent to 'Guess') may be used to represent the actual target or a 
-- possible target, depending on context. 
type Guess = [Location]
type Target = [Location]
type Guesses = [Guess]
type GameState = Guesses

-- | The 'GameDataPair' is a 2-tuple containing a 'Guess' and a 'GameState' and
-- is the type used in functions such as 'initialGuess' and 'nextGuess'. It
-- represents the searcher's decision and holds information to assist the 
-- search algorithm in the game. 
type GameDataPair = (Guess, GameState)

-- | 'Feedback' is a 3-tuple of Ints that usually represents the number of 
-- guess locations that are distance 0, 1, and 2 away from their closest target 
-- location, respectively. This type is suitable to hold the information 
-- determined by the hider that is given to the searcher to provide feedback 
-- on the most recent guess made by the searcher, and is also used internally 
-- in the searcher's algorithm.
type Feedback = (Int, Int, Int)

-- | 'toLocation' takes a two-character String consisting of an uppercase letter
-- in 'A'..'H', and a number character in '1'..'4', representing a location on 
-- the 4x8 board. The returned value of this function is of type
-- @Maybe 'Location'@, and will give @Just@ the 'Location' if the string 
-- represents a valid location, otherwise it will return @Nothing@. 
toLocation :: String -> Maybe Location
toLocation [letter, number] =
  let row = numberToRow number in
  case row of
    Nothing -> Nothing
    Just row_num -> let column = letterToColumn letter in
      case column of
        Nothing -> Nothing
        Just col_num -> Just (Location col_num row_num)
toLocation _ = Nothing
  
-- | 'letterToColumn' takes a @Char@, @letter@, and returns a @Just 'Column'@ 
-- of the integer for use as the 'Column' part of the 'Location' if the letter
-- is a valid column coordinate for the board, otherwise it returns @Nothing@
letterToColumn :: Char -> Maybe Column
letterToColumn letter
  | elem letter ['A'..'H'] = Just $ ord letter - ord 'A' + 1
  | otherwise = Nothing
  
-- | 'numberToRow' takes a @Char@, @number@, and returns a @Just 'Row'@ of
-- the integer for use as the 'Row' part of the 'Location' if the number is
-- a valid row coordinate for the board, otherwise it returns @Nothing@
numberToRow :: Char -> Maybe Row
numberToRow number
  | elem number ['1'..'4'] = Just $ digitToInt number
  | otherwise = Nothing
  
-- | 'fromLocation' takes an input of 'Location' type, representing a location
-- on the board, and returns a two-character String with an uppercase letter
-- representing the column and an integer character representing the row.
fromLocation :: Location -> String
fromLocation (Location column row) = columnToLetter column : show row

-- | 'columnToLetter' takes a 'Column' (equivalent to @Int@), and returns the 
-- appropriately converted @Char@ (for a valid 'Column' values, this will be 
-- a letter in 'A'..'H')
columnToLetter :: Column -> Char
columnToLetter column = chr (column + ord 'A' - 1)

-- | The 'feedback' function takes 'Target' and  'Guess' types (equivalent to
-- two @[Location]@), the first being considered the target and the second
-- being considered the guess. 'feedback' returns a 'Feedback', which is a 
-- three-tuple of @Int@ type representing the number of guess locations that
-- are distance 0, 1, and 2 away from their closest target location,
-- respectively.
feedback :: Target -> Guess -> Feedback
feedback target guess = (dist0, dist1, dist2)
  where dists = map (closestDist target) guess
        [dist0, dist1, dist2] = [(length . filter (== x)) dists | x <- [0..2]]
        
-- | 'distance' takes two 'Location's and calculates the Chebyshev distance
-- between them, returning this distance as an @Int@.
distance :: Location -> Location -> Int
distance (Location col1 row1) (Location col2 row2)
  = maximum $ map abs [row2 - row1, col2 - col1]

-- | 'closestDist' calculates the minimum Chebyshev 'distance' between guessLoc,
-- a single 'Location' from a guess, and each 'Location' in the target. 
closestDist :: Target -> Location -> Int
closestDist target guessLoc = minimum $ map (distance guessLoc) target

-- | The 'initialGuess' function does not take any input arguments, and returns
-- 'GameDataPair', which is a two-tuple consisting of a 'Guess' (@[Location]@)
-- and 'GameState'. Since there is no prior feedback, the first guess is chosen
-- with the aim to narrow down the possible targets the most on average, and
-- for efficiency reasons this is hardcoded just for the initial guess.
-- The game state data, refered to here as the variable @combGuess@, is
-- initialised in this function as a list of every possible target (three
-- element list of 'Location's) which span all the combinations of length 3
-- of valid locations on the board.
initialGuess :: GameDataPair
initialGuess = ([Location 2 1, Location 1 3, Location 2 4], combGuess)
  where combGuess = [[locA, locB, locC] | locA <- locs, locB <- locs,
                     locC <- locs, locA < locB, locB < locC]
        locs = [Location col row | row <- [1..4], col <- [1..8]]
        
-- | 'nextGuess' takes a 'GameDataPair' (equivalent type to 
-- @([Location], GameState)@) containing the most recent guess @prevGuess@ and
-- previous game state @gameState@, along with the @guessFeedback@ of type
-- 'Feedback' (@(Int, Int, Int)@) which is the number of locations in the
-- previous guess that are of distance 0, 1 and 2 respectively away from
-- the actual target. 
nextGuess :: GameDataPair -> Feedback -> GameDataPair
nextGuess (prevGuess, gameState) guessFeedback = (newGuess, newGameState)
  where newGameState = updateGameState (prevGuess, gameState) guessFeedback
        newGuess = findBestGuess newGameState

-- | `updateGameState` takes a GameDataPair consisting of a tuple containing the
-- most recent `guess` (a list of Locations) and the list of targets 
-- (`possTargets`, which is of type GameState),
-- and the feedback `guessFb` from the most recent guess. It returns a list of targets 
-- that are consistent with the recent guess and feedback.
updateGameState :: GameDataPair -> Feedback -> GameState
updateGameState (guess, possTargets) guessFb = 
  [target | target <- possTargets, feedback target guess == guessFb]
  
-- | 'findBestGuess' takes @possTargets@ of type 'Guesses' (equivalent to
-- @[[Location]]@), representing all the guesses that are consistent with the
-- feedback from every guess in the game up to this point. 
-- It returns the 'Guess' which is predicted to have the least remaining
-- possible targets, with the aim being to choose the guess which narrows
-- down the leftover possible targets by the most to attempt to minimise the
-- number of guesses required to find the target.
findBestGuess :: Guesses -> Guess
findBestGuess possTargets =
  fst $ minimumBy (\(_,x) (_,y) -> compare x y) $ expectedRemaining possTargets

-- | 'expectedRemaining' takes @possTargets@ of type 'Guesses' representing all
-- the guesses that are consistent with the feedback from every guess in the
-- game up to this point.
-- It returns a list of 'Guess' and @Float@ pairs, which are each guess that
-- still has the possibility of being the target and the expected number of
-- remaining possible targets that the feedback from choosing this guess
-- could result in. 
expectedRemaining :: Guesses -> [(Guess, Float)]
expectedRemaining possTargets =
  [(guess, calculateExpected guess possTargets len) | guess <- possTargets]
    where len = length possTargets

-- | 'calculateExpected' takes a @guess@ (of type 'Guess'), @possTargets@ of
-- type 'Guesses' representing all the three element lists of 'Location's 
-- that are consistent with the feedback from every guess in the game up to
-- this point and so are still possible to be the actual target, and @len@ an
-- @Int@ for length of @possTargets@. 
-- It returns a @Float@, with the value as specified in the formula,
-- \[ \sum_{f\in F} \frac{\mathrm{count}(f)^2}{T} \]
calculateExpected :: Guess -> Guesses -> Int -> Float
calculateExpected guess possTargets len =
  let possFeedback = map (flip feedback guess) possTargets 
      groupedFeedback = (group . sort) possFeedback
      lenGroupsSq = map ((^2) . length) groupedFeedback in
  fromIntegral (sum lenGroupsSq) / (fromIntegral len)

