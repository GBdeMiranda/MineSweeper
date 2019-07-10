-- =================================================
-- Gabriel Brandao de Miranda ======================
-- 201565514B ======================================
-- =================================================

import qualified Data.Char as DC
import qualified Data.List as DL
import qualified Board as Bd
import System.Random (newStdGen)

-- criar funcao de round de jogada

-- round :: Bd.Board -> IO ()
-- round b = do
--   input <- prompt "Escolha uma posição: "
--   let b' = inputCommand input b
--   putStrLn $ show b'
--   if (win b')
--   then (do
--     putStrLn "Parabens! Voce venceu!"
--     return ()
--     )
--   else if (gameOver b')
--        then (do
--          putStrLn "Game Over!"
--          return ()
--          )
--        else runTurn b'

inputCommand :: Char -> Int -> Int -> Bd.Board -> Bd.Board
inputCommand '+' i j (Bd.Board theMatrix sizeX sizeY) = (Bd.Board (Bd.modifyMatrix '+' theMatrix i j ) sizeX sizeY)
inputCommand '-' i j (Bd.Board theMatrix sizeX sizeY) = (Bd.Board (Bd.modifyMatrix '-' theMatrix i j ) sizeX sizeY)
inputCommand '_' i j (Bd.Board theMatrix sizeX sizeY) = (Bd.Board (Bd.modifyMatrix '_' theMatrix i j ) sizeX sizeY)
inputCommand  _  _ _   b = b

handleInput :: String -> Bd.Board -> Bd.Board
handleInput (c:j:i:x) theBoard = inputCommand c (DC.digitToInt i) (DC.digitToInt j) theBoard

main = do 
        putStrLn "================================================="
        putStrLn "====== [DCC019] - LINGUAGEM DE PROGRAMAÇÃO ======"
        putStrLn "================================================="
        putStrLn "====== TRABALHO PRÁTICO  ---  CAMPO MINADO ======"
        putStrLn "================================================="
        putStrLn " "
        putStrLn " "
        putStrLn " Informe o número de linhas do tabuleiro: "
        sizeX <- getLine
        putStrLn " Informe o número de colunas do tabuleiro: "
        sizeY <- getLine
        putStrLn " Informe o número de minas do tabuleiro: "
        mineAmount <- readLn
        newSeed <- newStdGen
        let dim = (read sizeX, read sizeY)
        let board = Bd.Board { Bd.matrix = Bd.createBoard dim newSeed mineAmount, Bd.dimX = fst dim, Bd.dimY = snd dim }
        let currentBoard = Bd.showBoard board
        putStrLn currentBoard



--   <posição> posição a ser aberta A1, D4, B3
-- + <posição> indicação da posição a ser marcada como mina +D2, + C4
-- - <posição> indicação para desmarcar uma posição que está marcada como mina -D2, -C4, -A1