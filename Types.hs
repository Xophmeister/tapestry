module Types where

-- Width::Int Height::Int Data::[Float]
data ImageData = ImageData Int Int [Float]
  deriving (Show)

type Palette = String
