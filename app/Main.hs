{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Main where

--import State( PuzzleState(Game) )
import Data.Time.Clock
import Data.Time.LocalTime
import Graphics.Gloss
import Graphics.Gloss.Data.Display
import Graphics.Gloss.Data.Picture
import Graphics.Gloss.Data.ViewPort
import Graphics.Gloss.Interface.Pure.Game
import System.Random
import Graphics.Gloss.Data.Picture
import Data.List (maximumBy, minimumBy)

width, height, xoffset, yoffset :: Int
width = 1280
height = 960
xoffset = 300
yoffset = 50

window :: Display
window = InWindow "Puzzle-Game" (width, height) (xoffset, yoffset)

background :: Color
background = makeColor 0.2 0.2 0.2 0.2

gridSize :: Float
gridSize = 48

high = rectangleWire gridSize gridSize

data PuzzleState = Game{
    grids :: [Int],
    crctConfig :: [Int],
    rows, cols :: Int,
    posx, posy :: Float,
    input :: Int,
    stage :: Float,
    caption :: [Char],
    best :: Int,
    randGen :: StdGen,
    gridPics::[Picture]

}

mapSquareToPicNumber :: Square -> Int

mapSquareToPicNumber (Square 0 0 0 0) = 0
mapSquareToPicNumber (Square 0 0 0 1) = 1
mapSquareToPicNumber (Square 0 0 1 0) = 2
mapSquareToPicNumber (Square 0 0 1 1) = 3
mapSquareToPicNumber (Square 0 1 0 0) = 4
mapSquareToPicNumber (Square 0 1 0 1) = 5
mapSquareToPicNumber (Square 0 1 1 0) = 6
mapSquareToPicNumber (Square 0 1 1 1) = 7
mapSquareToPicNumber (Square 1 0 0 0) = 8
mapSquareToPicNumber (Square 1 0 0 1) = 9
mapSquareToPicNumber (Square 1 0 1 0) = 10
mapSquareToPicNumber (Square 1 0 1 1) = 11
mapSquareToPicNumber (Square 1 1 0 0) = 12
mapSquareToPicNumber (Square 1 1 0 1) = 13
mapSquareToPicNumber (Square 1 1 1 0) = 14
mapSquareToPicNumber (Square 1 1 1 1) = 15
mapSquareToPicNumber _ = 0

mapNumberToSquare :: Int -> Square

mapNumberToSquare 0 = (Square 0 0 0 0)
mapNumberToSquare 1 = (Square 0 0 0 1)
mapNumberToSquare 2 = (Square 0 0 1 0)
mapNumberToSquare 3 = (Square 0 0 1 1)
mapNumberToSquare 4 = (Square 0 1 0 0)
mapNumberToSquare 5 = (Square 0 1 0 1)
mapNumberToSquare 6 = (Square 0 1 1 0)
mapNumberToSquare 7 = (Square 0 1 1 1)
mapNumberToSquare 8 = (Square 1 0 0 0)
mapNumberToSquare 9 = (Square 1 0 0 1)
mapNumberToSquare 10 = (Square 1 0 1 0)
mapNumberToSquare 11 = (Square 1 0 1 1)
mapNumberToSquare 12 = (Square 1 1 0 0)
mapNumberToSquare 13 = (Square 1 1 0 1)
mapNumberToSquare 14 = (Square 1 1 1 0)
mapNumberToSquare 15 = (Square 1 1 1 1)
mapNumberToSquare _ = (Square 0 0 0 0)

-- | Rotates the puzzle piece clockwise 
rotateClockwise :: Int -> Int
rotateClockwise 0 = 0 -- 0
rotateClockwise 1 = 2 -- 1
rotateClockwise 2 = 4 -- 1
rotateClockwise 3 = 6 -- 2
rotateClockwise 4 = 8 -- 1
rotateClockwise 5 = 10 -- 2
rotateClockwise 6 = 12 -- 2
rotateClockwise 7 = 14 -- 3
rotateClockwise 8 = 1 -- 1
rotateClockwise 9 = 3 -- 2
rotateClockwise 10 = 5 -- 2
rotateClockwise 11 = 7 -- 3
rotateClockwise 12 = 9 -- 2
rotateClockwise 13 = 11 -- 3
rotateClockwise 14 = 13 -- 2
rotateClockwise a = a -- 4

--  | Function to rotate one piece to another piece
turn :: Int -> Int -> Int
turn 0 b = b
turn 1 b = rotateClockwise b
turn 2 b = rotateClockwise (rotateClockwise b)
turn 3 b = rotateClockwise (rotateClockwise (rotateClockwise b))
turn a b = turn (a`rem` 4) b

--  | Takes a complete puzzle as input and shuffles it
shuffle:: [Int] -> StdGen -> [Int]
shuffle [] g = []
shuffle (a:as) g = turn b a:shuffle as newGen where
    (b, newGen) = randomR (0,3) g

-- | Render one piece of puzzle
renderGrid
  :: Picture  -- ^ puzzle pic
  -> Float    -- ^ x position
  -> Float    -- ^ y position
  -> Int      -- ^ index
  -> Color    -- ^ color
  -> Picture
renderGrid pics x y a clr = translate (x * gridSize + x -2 * gridSize) (y * gridSize + y -2 * gridSize) $ color clr pics

g = mkStdGen 9

initState :: PuzzleState
initState =
  Game
    { grids = [],
      crctConfig = [1],
      rows = 4,
      cols = 4,
      posx = 1.5,
      posy = 1.5,
      input = 0,
      stage = 5,
      caption = "",
      best = 0,
      randGen = mkStdGen 100,
      gridPics = []
    }

getNextPos :: Float -> Float -> Float -> (Float, Float)
getNextPos x y columns =
    if x == columns
        then (1,  (y+1))
    else
        ((x+1), y)

-- Checks completeness of the puzzle
checkPuzzle 
  :: [Square]  -- ^ puzzle pieces
  -> [Square]  -- ^ previous level
  -> [Square]  -- ^ current row, initially empty
  -> Square    -- ^ first element to compare
  -> Float  -- ^ current row
  -> Float  -- ^ total rows
  -> Float  -- ^ current column
  -> Bool   -- ^ result

checkPuzzle [] [] presrow prev row mrow cols = True
checkPuzzle grids [] presrow prev row mrow col
  | row < mrow = checkPuzzle grids presrow [] createNewTunnel (row+1) mrow 1
  | null grids = True
  | otherwise = False
checkPuzzle (g:gs) (t:ts) presrow prev row mrow col  = lastCheck && checkPuzzle gs ts (presrow++[g]) g row mrow (col+1) where
    lastCheck
      | row == 1 && col == 1 && sideA g == 0 && sideB g == 0 = True

      | row == mrow && null ts && sideD g == 0 && sideC g == 0 && (sideA g == sideC prev) && (sideB g == sideD t) = False 
      | row == mrow && (sideA g == sideC prev) && (sideB g == sideD t) = True
      | null ts && sideC g == 0 = True
      | row == 1 && null ts && (sideA g == sideC prev) && (sideB g == sideD t) && sideC g == 0 && sideB g == 0 = True
      | row/=1 && not (null ts) && (sideA g == sideC prev) && (sideB g == sideD t) = True
      | otherwise = False
checkPuzzle grids tunnels presrow prev row mrow col = False


-- | Recursively generates each piece of the grid
renderPuzzle
  :: [Picture] -- ^ all pictures
  -> [Int]     -- ^ puzzle grids
  -> Float     -- ^ x point
  -> Float     -- ^ y point
  -> Float     -- ^ rows
  -> Float     -- ^ columns
  -> Float     -- ^ position x
  -> Float     -- ^ position y
  -> Int       -- ^ corner value
  -> String    -- ^ caption to write on screen
  -> [Picture] -- ^ puzzle

renderPuzzle pics [] x y rows columns px py bes capt = [translate (-300) 300 $ scale 0.5 0.5 $ color white $ text capt]
renderPuzzle pics (a : xs) x y rows columns px py bes capt = (if x == (intToFloat $ floor px) && y == (intToFloat $ floor py) then [renderGrid (pics !! a) x y a white] ++ [renderGrid (rectangleWire 49 49) x y a white] else [renderGrid (pics !! a) x y a black]) ++ renderPuzzle pics xs (fst (getNextPos x y columns)) (snd (getNextPos x y columns)) rows columns px py bes capt

intToFloat :: Int -> Float
intToFloat = fromIntegral

-- | Function to load all screens
renderState :: [Picture] -> PuzzleState -> Picture
renderState a s
  | tempStage == 4 = winnerHighlighting
  | otherwise = currentPuzzle
  where
    tempStage = stage s
    currentPuzzle = pictures (renderPuzzle a (grids s) 1 1 (intToFloat $ rows s) (intToFloat $ cols s) (posx s) (posy s) (best s) (caption s))
    winnerHighlighting = pictures [ color white $ rectangleWire (48 * intToFloat rowN ) (48 * intToFloat colN) ]
    rowN = rows s
    colN = cols s


data Square = Square
  { sideA :: Int
  , sideB :: Int
  , sideC :: Int
  , sideD :: Int
  }

createSquare :: Int -> Int -> Int -> Int -> Square
createSquare = Square

createNewTunnel = Square 0 0 0 0

toFloatToInt :: Float -> Int
toFloatToInt = round

-- Function which generate the correct puzzle
generateCorrectPuzzle
  :: [Square]            -- ^ buffer to add puzzle pieces
  -> [Square]            -- ^ prev level
  -> [Square]            -- ^ current puzzle row's pieces
  -> Square              -- ^ number of tunels available at current position
  -> Float               -- ^ current row number
  -> Float               -- ^ number of rows in a puzzle
  -> Float               -- ^ current column number
  -> StdGen              -- ^ generator
  -> ([Square], StdGen)  -- ^ returns right configuration and next seed

generateCorrectPuzzle buffer [] row tunnels rowNumber rows columnNumber g' = if rowNumber < rows then generateCorrectPuzzle (buffer++row) row [] createNewTunnel (rowNumber+1) rows 1 g' else (buffer++row, g')

generateCorrectPuzzle buffer (a:as) row tunnels rowNumber rows columnNumber g' = generateCorrectPuzzle buffer as (row++[newElement]) newElement rowNumber rows (columnNumber+1) newGen where
    (c, newRand) = randomR (0, 10) g'
    (d, newGen) = randomR (0, 15) newRand

    randC = (toFloatToInt c) `rem` 2
    randD = (toFloatToInt d) `rem` 2  
    newElement | rowNumber == 1 && columnNumber == 1 = createSquare 0 0 randC randD
               | rowNumber == rows && columnNumber == 1 = createSquare 0 (sideD a) randD 0
               | rowNumber == rows && null as = createSquare (sideC tunnels) (sideD a) 0 0
               | rowNumber == 1 && null as = createSquare (sideC tunnels) 0 0 randD
               | rowNumber == 1 = createSquare (sideC tunnels) 0 randC randD
               | columnNumber == 1 = createSquare 0 (sideD a) randC randD
               | null as = createSquare (sideC tunnels) (sideD a) 0 randC
               | rowNumber == rows = createSquare (sideC tunnels) (sideD a) randC 0
               | otherwise = createSquare (sideC tunnels) (sideD a) randC randD

generatePuzzle
  :: [Square]                -- ^ buffer to add puzzle pieces
  -> [Square]                -- ^ prev row
  -> [Square]                -- ^ current puzzle row's pieces
  -> Square                  -- ^ number of tunels available at current position
  -> Float                   -- ^ current row number
  -> Float                   -- ^ number of rows in a puzzle
  -> Float                   -- ^ current column number
  -> StdGen                  -- ^ current puzzle gen state
  -> ([Int], [Int], StdGen)  -- ^ returns correct configuration, shuffled grids and new seed
generatePuzzle buffer starting rowPieces tunnels rowNumber allRows columnNumber gen = (crctConfig, shuffled, newSeed)
  where
    (coorectSquareConfig, newSeed) = generateCorrectPuzzle buffer starting rowPieces tunnels rowNumber allRows columnNumber gen
    crctConfig = map mapSquareToPicNumber coorectSquareConfig
    shuffled = shuffle crctConfig newSeed


-- | Updates the state frame
updateState :: Float -> PuzzleState -> PuzzleState
updateState s m
  | st == 5 = m {crctConfig = correctConfig, grids = temp2, randGen = temp3, stage = 3.4}
  | grids m == crctConfig m = m {stage = 4}
  | checkPuzzle (map mapNumberToSquare (grids m)) buffer [] createNewTunnel 1 rows' 1 = m {stage = 4}
  | userInput == 1 && posy m < rows' = m {posy = posy m + 3 * s}
  | userInput == 4 && posy m >= 2 = m {posy = posy m - 3 * s}
  | userInput == 2 && posx m < columns' = m {posx = posx m + 3 * s}
  | userInput == 8 && posx m >= 2 = m {posx = posx m - 3 * s}
  | otherwise = m
  where
    userInput = input m
    st = stage m
    rows' = intToFloat $ rows m
    columns' = intToFloat $ cols m
    buffer = [createNewTunnel | x <- [1 .. (cols m)]]
    (correctConfig, temp2, temp3) =
      if st == 5
        then generatePuzzle [] buffer [] createNewTunnel 1 rows' 1 (randGen m)
        else ([0], [0], g)

-- | Rotate the i-th element in the list
changeN :: Float -> [Int] -> [Int]
changeN _ [] = []
changeN 0 (a : as) = rotateClockwise a : as
changeN i (a : as) = a : changeN (i -1) as


-- | Function takes the input from the user and updates the window accordingly
handleInput :: Event -> PuzzleState -> PuzzleState
handleInput (EventKey (SpecialKey KeyUp) Down _ _) game = game {input = 1}
handleInput (EventKey (SpecialKey KeyDown) Down _ _) game = game {input = 4}
handleInput (EventKey (SpecialKey KeyRight) Down _ _) game = game {input = 2}
handleInput (EventKey (SpecialKey KeyLeft) Down _ _) game = game {input = 8}
handleInput (EventKey (SpecialKey KeyUp) Up _ _) game = if input game == 1 then game {input = 0} else game
handleInput (EventKey (SpecialKey KeyDown) Up _ _) game = if input game == 4 then game {input = 0} else game
handleInput (EventKey (SpecialKey KeyRight) Up _ _) game = if input game == 2 then game {input = 0} else game
handleInput (EventKey (SpecialKey KeyLeft) Up _ _) game = if input game == 8 then game {input = 0} else game
handleInput (EventKey (SpecialKey KeyEnter) Down _ _) game = if posx game > 0 then game {grids = changeN (intToFloat currentPosition) (grids game)} else game
  where
    currentPosition = (floor (posy game) -1) * (rows game) + floor (posx game) -1
handleInput (EventKey (Char 'q') (Up) _ _) game = game {stage = 5}
handleInput _ game = game

fps :: Int
fps = 120

main :: IO ()
main = do
  shape0 <- loadBMP "./src/myShapes/shape0.bmp"
  shape1 <- loadBMP "./src/myShapes/shape1.bmp"
  shape2 <- loadBMP "./src/myShapes/shape2.bmp"
  shape3 <- loadBMP "./src/myShapes/shape3.bmp"
  shape4 <- loadBMP "./src/myShapes/shape4.bmp"
  shape5 <- loadBMP "./src/myShapes/shape5.bmp"
  shape6 <- loadBMP "./src/myShapes/shape6.bmp"
  shape7 <- loadBMP "./src/myShapes/shape7.bmp"
  shape8 <- loadBMP "./src/myShapes/shape8.bmp"
  shape9 <- loadBMP "./src/myShapes/shape9.bmp"
  shape10 <- loadBMP "./src/myShapes/shape10.bmp"
  shape11 <- loadBMP "./src/myShapes/shape11.bmp"
  shape12 <- loadBMP "./src/myShapes/shape12.bmp"
  shape13 <- loadBMP "./src/myShapes/shape13.bmp"
  shape14 <- loadBMP "./src/myShapes/shape14.bmp"
  shape15 <- loadBMP "./src/myShapes/shape15.bmp"
  let pics = [shape0] ++ [shape1] ++ [shape2] ++ [shape3] ++ [shape4] ++ [shape5] ++ [shape6] ++ [shape7] ++ [shape8] ++ [shape9] ++ [shape10] ++ [shape11] ++ [shape12] ++ [shape13] ++ [shape14] ++ [shape15]
  play window background fps initState (renderState pics) handleInput updateState
