-- =================================================
-- Gabriel Brandao de Miranda ======================
-- 201565514B ======================================
-- =================================================

module Board ( Board(..) 
, mineCounter
, showSquare
, getSquare 
, oneForMine
, createBoard
, showBoard
, modifySquare
, modifyRow
, modifyMatrix
) where

import qualified System.Random as SR
import qualified Data.Char as DC
import qualified Data.List as DL
import qualified Square as Sq

data Board = Board { matrix :: [[Sq.Square]] , dimX :: Int , dimY :: Int }

getSquare :: Board -> Int -> Int -> Sq.Square
getSquare ( Board theMatrix sizeX sizeY ) x y  
    | (x > 0 && x <= sizeX) && (y > 0 && y <= sizeY) = (theMatrix!!x)!!y
    | otherwise    = Sq.Empty

oneForMine :: Sq.Square-> Int
oneForMine (Sq.Square _ _ _ True ) = 1
oneForMine _ = 0

createBoard :: SR.RandomGen genRand => (Int, Int) -> genRand-> Int -> [[Sq.Square]]
createBoard (sizeX,sizeY) randomSeed numberOfMines = 
    foldr (\row b -> ( auxCreation (take numberOfMines (DL.nub (SR.randomRs (1, sizeX*sizeY) randomSeed) ) ) sizeY row) : b) [] [1..sizeX]
        where auxCreation randomList sizeList jj =foldr (\ii a -> Sq.Square { Sq.posX = ii, Sq.posY = jj, Sq.state = Sq.Closed , Sq.mine = (elem (ii + (jj-1)*sizeList) randomList) } : a) [] [1..sizeList]

showBoard :: Board -> String
showBoard theBoard@( Board theMatrix sizeX sizeY ) =
    foldr (\row b -> auxShow theBoard sizeY (theMatrix!!row) ++ b) "\n   " [0..sizeX-1]
        where auxShow theBoard sizeList theLine = foldr (\col a -> (showSquare theBoard (theLine!!col)) ++ a) " \n" [0..sizeList-1]


modifySquare ::  Char -> Sq.Square -> Sq.Square
modifySquare '+' square = Sq.markSquare square
modifySquare '-' square = Sq.unmarkSquare square
modifySquare '_' square = Sq.openSquare square

modifyRow :: Char -> [Sq.Square] -> Int -> [Sq.Square]
modifyRow command row index = take index row ++ [ (modifySquare command (row!!index) ) ] ++ drop (index+1) row

modifyMatrix :: Char -> [[Sq.Square]] -> Int -> Int -> [[Sq.Square]]
modifyMatrix command matrix posX posY = take posX matrix ++ [ (modifyRow command (matrix!!posX) posY) ] ++ drop (posX+1) matrix


-- ==========================================================================================================================================================================================
-- Should be in the Square module, but it would form a cycle ================================================================================================================================
-- ==========================================================================================================================================================================================
mineCounter :: Board -> Sq.Square -> Int
mineCounter theBoard (Sq.Square posX posY _ _ ) = oneForMine (getSquare theBoard (posX-1) posY) + oneForMine (getSquare theBoard (posX+1) posY) + oneForMine (getSquare theBoard posX (posY-1) ) + oneForMine (getSquare theBoard posX (posY+1) )

showSquare :: Board -> Sq.Square -> String
showSquare _ (Sq.Square _ _ Sq.Closed _ ) = " *" 
showSquare theBoard theSquare@(Sq.Square _ _ Sq.Open _ ) = ([' ', (DC.intToDigit (mineCounter theBoard theSquare) )] :: String ) -- Devia ser o numero de vizinhos com bombas

-- ==========================================================================================================================================================================================
-- ==========================================================================================================================================================================================
-- ==========================================================================================================================================================================================