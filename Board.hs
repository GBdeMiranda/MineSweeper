-- =================================================
-- Gabriel Brandao de Miranda ======================
-- 201565514B ======================================
-- =================================================

module Board ( Board(..) 
    -- , appendNewRandomNumber
    -- , generateNRandomNumbers
, createBoard
, showSquare
, showBoard
) where

import System.Random (randomR, RandomGen, mkStdGen, newStdGen)
import qualified Data.Char as DC
import qualified Square as Sq

-- appendNewRandomNumber :: RandomGen g => Set Int -> (Int, Int) -> g -> (Set Int, g)
-- appendNewRandomNumber s (a, b) g = if (length newSet > length s)
--                                    then (newSet, newG)
--                                    else appendNewRandomNumber s (a, b) newG
--                                    where newSet = insert r s
--                                          (r, newG) = randomR (a, b) g

-- generateNRandomNumbers :: RandomGen g => (Int, Int) -> Int -> g -> Set Int
-- generateNRandomNumbers (a, b) n g0 = fst $ foldr (\_ (s, g) -> appendNewRandomNumber s (a, b) g) (empty, g0) [1..n]

-- newRand = newStdGen
-- randomR (1,100) newRand

data Board = Board { matrix :: [[Sq.Square]] , dimX :: Int , dimY :: Int }

createBoard :: (Int, Int) -> [[Sq.Square]]
createBoard (sizeX,sizeY) = 
    foldr (\row b -> auxCreation sizeY row : b) [] [1..sizeX]
        where auxCreation sizeList jj = foldr (\ii a -> Sq.Square { Sq.posX = ii, Sq.posY = jj, Sq.state = Sq.Closed , Sq.mine = False} : a) [] [1..sizeList]

showSquare :: Sq.Square -> String
showSquare (Sq.Square _ _ Sq.Closed _ ) = " *" 
showSquare (Sq.Square _ _ Sq.Opened _ ) = " 0"  -- Devia ser o numero de vizinhos com bombas

showBoard :: Board -> String
showBoard ( Board theMatrix sizeX sizeY ) =
    foldr (\row b -> auxShow sizeY (theMatrix!!row) ++ b) "\n   " [0..sizeX-1]
        where auxShow sizeList theLine = foldr (\col a -> showSquare (theLine!!col) ++ a) " \n" [0..sizeList-1]

-- showRow b (c:cs) s = s ++ rowNumber ++ "  " ++ showCells ++ "\n"
--                       where rowNumber = [row $ position c]
--                             showCells = foldl showCell "" (c:cs)
--                             showCell = \acc c -> acc ++ [cellToChar c b] ++ " "

