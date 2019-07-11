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
    | ((x-1) >= 0 && (x-1) < sizeX) && ( (y-1) >= 0 && (y-1) < sizeY) = (theMatrix!!(x-1))!!(y-1)
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
    foldr (\row b -> auxShow row theBoard sizeY (theMatrix!!row) ++ b) 
        ("   " ++ ( DL.intersperse ' '  (take sizeY (['a'..'z']++['A'..'Z']) ) ) ++ " \n" ) [sizeX-1,(sizeX-2)..0] -- ++ map (\x -> ['A', x] ) ['A'..'Z'] ++ map (\x -> ['B', x] ) ['A'..'Z'] ) ) ) 
            where auxShow indexRow theBoard sizeList theLine = [DC.intToDigit ( div (indexRow+1) 10 ), DC.intToDigit ( mod (indexRow+1) 10 ) ] ++ foldr (\col a -> (showSquare theBoard (theLine!!col)) ++ a) " \n" [0..sizeList-1]

modifyRow :: Char -> [Sq.Square] -> Int -> [Sq.Square]
modifyRow command row indexY = take indexY row ++ [ (Sq.modifySquare command (row!!indexY) ) ] ++ drop (indexY+1) row

modifyMatrix :: Char -> [[Sq.Square]] -> Int -> Int -> [[Sq.Square]]
modifyMatrix command matrix posX posY = take (posX-1) matrix ++ [ (modifyRow command (matrix!!(posX-1)) posY) ] ++ drop posX matrix

-- ==========================================================================================================================================================================================
-- Should be in the Square module, but it would form a cycle ================================================================================================================================
-- ==========================================================================================================================================================================================
mineCounter :: Board -> Sq.Square -> Int
mineCounter theBoard (Sq.Square posX posY _ _ ) = oneForMine (getSquare theBoard (posX-1) posY) + oneForMine (getSquare theBoard (posX+1) posY) + oneForMine (getSquare theBoard posX (posY-1) ) + oneForMine (getSquare theBoard posX (posY+1) )

showSquare :: Board -> Sq.Square -> String
showSquare _ (Sq.Square _ _ Sq.Closed _ ) = " *"
showSquare theBoard theSquare@(Sq.Square _ _ Sq.Open _ ) = ([' ', (DC.intToDigit (mineCounter theBoard theSquare) )] :: String ) -- Devia ser o numero de vizinhos com bombas
showSquare _ (Sq.Square _ _ Sq.Marked _ ) = " B"
-- ==========================================================================================================================================================================================
-- ==========================================================================================================================================================================================
-- ==========================================================================================================================================================================================