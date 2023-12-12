module Menu.Controls where

data HDirection
  = Left
  | Right
  deriving (Eq, Show)

data VDirection
  = Up
  | Down
  deriving (Eq, Show)

data Direction
  = Horizontal HDirection
  | Vertical VDirection
  deriving (Eq, Show)

data KeyAction
  = Enter
  | Quit
  | Move Direction
  deriving (Eq, Show)
