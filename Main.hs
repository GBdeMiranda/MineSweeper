-- =================================================
-- Gabriel Brandao de Miranda ======================
-- 201565514B ======================================
-- =================================================

import qualified Data.Char as DC
import qualified Data.List as DL
import qualified Board as Bd
import System.Random (newStdGen)
import System.IO

data GameState  = Neutral | Invalid | Victory  deriving( Ord, Eq, Show )

data Controller = Controller { mines :: Int , markeds :: Int , opens :: Int , boardSize :: Int , invalid :: Bool }

data Game = Game { board :: Bd.Board , controller :: Controller }

-- putStr :: String -> IO ()  
-- putStr [] = return ()  
-- putStr (x:xs) = do  
--     putChar x  
--     putStr xs  

numberToInt :: String -> Int
numberToInt [a,b] 
    | (DC.digitToInt a) >= 0 && (DC.digitToInt a) < 10 && (DC.digitToInt b) >= 0 && (DC.digitToInt b) < 10 = (DC.digitToInt a)*10 + (DC.digitToInt b)
    | otherwise = error "Entrada Inválida! <numberToInt>"
numberToInt theError =  error "Entrada Inválida!"

gameController :: Controller -> GameState
gameController (Controller theMines theMarkeds theOpens theBoardSize theInvalid)
    | theBoardSize - theOpens == theMines = Victory
    | theInvalid = Invalid
    | otherwise = Neutral

inputCommand :: Char -> Int -> Int -> Bd.Board -> Controller -> Game
inputCommand '+' i j (Bd.Board theMatrix sizeX sizeY) (Controller a m b c d) = (Game (Bd.Board (Bd.modifyMatrix '+' theMatrix i j ) sizeX sizeY) (Controller a (m+1) b c d) )
inputCommand '-' i j (Bd.Board theMatrix sizeX sizeY) (Controller a m b c d) = (Game (Bd.Board (Bd.modifyMatrix '-' theMatrix i j ) sizeX sizeY) (Controller a (m-1) b c d) )
inputCommand '_' i j (Bd.Board theMatrix sizeX sizeY) (Controller a b o c d) = (Game (Bd.Board (Bd.modifyMatrix '_' theMatrix i j ) sizeX sizeY) (Controller a b (o+1) c d) )
inputCommand  _  _ _  b c = error "Entrada Inválida!"

-- INVERT LINE AND COLUMN TO FIND THE LINE FIRST
handleCommand :: String -> Game -> Game
handleCommand (k:j:i) ( Game theBoard theController@( Controller a b c d e ) )
    | (k == '+' || k == '-' ||  k == '_' ) && (j <= 'z' && j >= 'a' || j <= 'Z' && j >= 'A') && (numberToInt i < 100) =
        inputCommand k   ( numberToInt i  ) ((DC.digitToInt j) - (DC.digitToInt 'a')) theBoard theController
    -- | i == ""                && (k < 'z' && k > 'a' || k < 'Z' && k > 'A') && (numberToInt [j] < 100) = 
    --     inputCommand '_' (DC.digitToInt j ) ((DC.digitToInt k) - (DC.digitToInt 'a')) theBoard theController
    | otherwise = ( Game theBoard ( Controller a b c d True ) ) -- error ("Entrada Inválida! <handleCommand> k=" ++ [k] ++ "; j=" ++ [j] ++ "; i =" ++ [(DC.intToDigit (numberToInt i))] )
handleCommand _ ( Game theBoard theController@( Controller a b c d e ) ) = ( Game theBoard ( Controller a b c d True ) )

currentRound :: Game -> IO ()
currentRound theGame@(Game theBoard ctrler ) = do
    let currentBoard = Bd.showBoard theBoard
    putStrLn " "
    putStrLn "  ==========================="
    putStrLn "  ======== TABULEIRO ========"
    putStrLn "  ==========================="
    putStrLn currentBoard
    putStr " Informe seu comando: "
    command <- getLine
    let theGame'@(Game theBoard' ctrler ) = handleCommand command theGame
    if (gameController ctrler) == Invalid
        then do
            putStrLn " "
            putStrLn " "
            putStrLn "  COMANDO INVÁLIDO, COLOQUE NO FORMATO ESPECIFICADO  "
            putStrLn " "
            putStrLn "====================================================="
            putStrLn "==================    COMANDOS     =================="
            putStrLn "====================================================="
            putStrLn "|| _ <posição> || Abrir Posição Ex.: A01, D44, B13 ||"
            putStrLn "|| + <posição> || Marcar Posição Ex.: +D92, +C04   ||"
            putStrLn "|| - <posição> || Desmarcar Posição Ex.:-D02, -C40 ||"
            putStrLn "====================================================="
            currentRound theGame'
    else if (gameController ctrler) == Victory
        then do
            putStrLn " \n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n  VOCÊ VENCEU! \n\n\n\n\n\n\n\n\n\n\n\n\n\n "
            return ()
        else currentRound theGame'

openning :: IO ()
openning = do
        putStrLn " "
        putStr " Informe o número de linhas do tabuleiro   (MAX 99) m: "
        sizeX <- getLine
        if ( (read sizeX) > 99 || (read sizeX) < 1 )
            then do
            putStrLn "  "
            putStrLn " ------------------------------ ERRO ------------------------------ "
            putStrLn " Respeite o intervalo permitido para o número de linhas [1,99] "
            putStrLn " "
            putStrLn " "
            openning
            return ()
        else do
            putStr " Informe o número de colunas do tabuleiro  (MAX 27) n: "
            sizeY <- getLine
            if ( (read sizeY) > 27 || (read sizeY) < 1 )
                then do
                putStrLn "  "
                putStrLn " ------------------------------ ERRO ------------------------------ "
                putStrLn " Respeite o intervalo permitido para o número de colunas [1,27] "
                putStrLn " "
                putStrLn " "
                openning
                return ()
            else do
                putStr " Informe o número de minas do tabuleiro (MAX m*n/2) k: "
                mineAmount <- readLn
                let {dim = (read sizeX, read sizeY)}
                let {maxMines = (fst dim)*(div (snd dim) 2) }
                if ( mineAmount > maxMines || mineAmount < 1 )
                    then do
                    putStrLn "  "
                    putStrLn " ------------------------------ ERRO ------------------------------ "
                    putStrLn (" Insira um valor no intervalo [1," ++ (show maxMines) ++ "] para o número de minas ")::IO ()
                    putStrLn " "
                    putStrLn " "
                    openning
                    return ()
                else do
                    newSeed <- newStdGen
                    let controller = Controller { mines = mineAmount , markeds = 0 , opens = 0 , boardSize = (fst dim)*(snd dim) , invalid = False}
                    let board = Bd.Board { Bd.matrix = Bd.createBoard dim newSeed mineAmount, Bd.dimX = fst dim, Bd.dimY = snd dim }
                    putStrLn " "
                    putStrLn " "
                    putStrLn "====================================================="
                    putStrLn "==================    COMANDOS     =================="
                    putStrLn "====================================================="
                    putStrLn "|| _ <posição> || Abrir Posição Ex.: A01, D44, B13 ||"
                    putStrLn "|| + <posição> || Marcar Posição Ex.: +D92, +C04   ||"
                    putStrLn "|| - <posição> || Desmarcar Posição Ex.:-D02, -C40 ||"
                    putStrLn "====================================================="
                    putStrLn " "
                    putStrLn " "
                    currentRound (Game board controller)
                    return ()

main = do 
    putStrLn "====================================================="
    putStrLn "======== [DCC019] - LINGUAGEM DE PROGRAMAÇÃO ========"
    putStrLn "====================================================="
    putStrLn "======== TRABALHO PRÁTICO  ---  CAMPO MINADO ========"
    putStrLn "====================================================="
    openning
