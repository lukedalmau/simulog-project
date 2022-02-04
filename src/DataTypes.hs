module DataTypes where

data Coord = Coord {x :: Int, y :: Int} deriving (Eq, Show)

data Env = Env
  { -- |
    env_size :: (Int, Int),
    -- |
    env_turn :: Int,
    -- |
    env_random_change :: Int,
    -- |
    env_elements :: [Element]
  }

data Element = Element
  { element_pos :: Coord,
    element_type :: ElementType,
    element_id :: Int,
    element_skill :: Bool
  }
  deriving (Show)


data ElementType = Baby | Dirt | Robot | PlayPen | Obstacle deriving (Show, Eq)

instance Show Env where
  -- draw the entire board tiles in ascii and include the elements on it
  show env = unlines $ map showRow [0 .. (fst $ env_size env) - 1]
    where
      showRow row =
        [showTile (env_elements env) (Coord row col) | col <- [0 .. (snd $ env_size env) - 1]]
      showTile elements (Coord row col) =
        case findElement elements (Coord row col) of
          Nothing -> '.'
          Just e ->
            case element_type e of
              Baby -> 'B'
              Dirt -> 'D'
              Robot -> 'R'
              PlayPen -> 'P'
              Obstacle -> 'O'

findElement :: [Element] -> Coord -> Maybe Element
findElement elements coord =
  case filter (\e -> element_pos e == coord) elements of
    [] -> Nothing
    (e:_) -> Just e

          

-- define maximum and minimum limit of random number

random_max :: Int
random_max = 100

random_min :: Int
random_min = 3

-- define size of the environment
env_size_x :: Int
env_size_x = 10

env_size_y :: Int
env_size_y = 10

envSize :: (Int, Int)
envSize = (env_size_x, env_size_y)
