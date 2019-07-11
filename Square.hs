-- =================================================
-- Gabriel Brandao de Miranda ======================
-- 201565514B ======================================
-- =================================================

module Square ( State(..)
, Square(..)
, markSquare
, unmarkSquare
, openSquare
, modifySquare
) where

import qualified Data.Char as DC

data State = Closed | Open | Marked  deriving( Eq, Show )

data Square = Empty | Square { posX :: Int, posY :: Int , state :: State , mine :: Bool } deriving ( Show, Eq )

markSquare :: Square -> Square
markSquare   ( Square a b c d )        = ( Square a b Marked d )

unmarkSquare :: Square -> Square
unmarkSquare ( Square a b c d )        = ( Square a b Closed d )

openSquare :: Square -> Square
openSquare ( Square a b c True )  = error "\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n MINA ATIVADA! \n VOCÊ PERDEU! \n\n\n\n\n\n\n"
openSquare ( Square a b c False ) = ( Square a b Open False )

modifySquare ::  Char -> Square -> Square
modifySquare '+' square = markSquare square
modifySquare '-' square = unmarkSquare square
modifySquare '_' square = openSquare square