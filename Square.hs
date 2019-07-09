-- =================================================
-- Gabriel Brandao de Miranda ======================
-- 201565514B ======================================
-- =================================================

module Square ( State(..)
, Square(..)
) where

import qualified Data.Char as DC

data State = Closed | Opened | Marked  deriving(Enum, Eq, Show)

-- data Position = Invalid | Position { posX :: Int, posY :: Int }

data Square = Square { posX :: Int, posY :: Int , state :: State , mine :: Bool } deriving (Show, Eq)
-- data Square = Empty | Square { position :: Position, state :: State} deriving (Show, Eq)