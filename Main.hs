-- =================================================
-- Gabriel Brandao de Miranda ======================
-- 201565514B ======================================
-- =================================================

import Data.Char as DC
import Data.List as DL
import qualified Square as Sq
import qualified Board as Bd

-- criar funcao de round de jogada

-- round :: Board -> IO ()
-- round b = do
--   input <- prompt "Escolha uma posição: "
--   let b' = processInput input b
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
        let dim = (read sizeX, read sizeY)
        let board = Bd.Board { Bd.matrix = Bd.createBoard dim , Bd.dimX = fst dim, Bd.dimY = snd dim }
        let currentBoard = Bd.showBoard board
        putStrLn currentBoard

        putStrLn " Informe o número de minas do tabuleiro: "
        m <- readLn
        if m == 4
            then putStrLn "Você acertou!"
            else putStrLn "Você errou!"