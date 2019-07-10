-- =================================================
-- Gabriel Brandao de Miranda ======================
-- 201565514B ======================================
-- =================================================

module Square ( State(..)
, Square(..)
, markSquare
, unmarkSquare
, openSquare
) where

import qualified Data.Char as DC

data State = Closed | Open | Marked  deriving( Eq, Show )

data Square = Empty | Square { posX :: Int, posY :: Int , state :: State , mine :: Bool } deriving ( Show, Eq )

markSquare :: Square -> Square
markSquare   ( Square a b c d )        = ( Square a b Marked d )

unmarkSquare :: Square -> Square
unmarkSquare ( Square a b c d )        = ( Square a b Closed d )

openSquare :: Square -> Square
openSquare ( Square a b Closed True )  = ( Square a b Open True  ) -- EXPLODE CORACAO NA MAIOR FELICIDADE
openSquare ( Square a b Closed False ) = ( Square a b Open False )