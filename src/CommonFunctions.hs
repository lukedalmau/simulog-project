module CommonFunctions where
import Control.Arrow
import Data.List
import System.Environment
import System.IO
import System.IO.Error
import System.IO.Unsafe
import System.Random

import DataTypes
    ( ElementType(Obstacle, Baby, Dirt, Robot, PlayPen),
      Element(..),
      Coord(Coord, x, y),
      random_max, Env (env_size) )

-- create random element type

randomElementType =
  let randomElementType = [Baby, Dirt, Robot, PlayPen, Obstacle] !! randomInt 0 4
   in randomElementType

-- create random int
randomInt :: Int -> Int -> Int
randomInt x y =
  let randomInt = unsafePerformIO (getStdRandom (randomR (x, y)))
   in randomInt

-- create random position
randomPosition env_size =
  let randomPosition = (randomInt 0 (fst env_size), randomInt 0 (snd env_size))
   in randomPosition

-- create a unique id for an element of the list without repetition
-- when creating the id check first if the id is already in the list
-- if it is then create a new id
randomElementId :: [Element] -> Int
randomElementId elements =
  let random_id = randomInt 0 random_max
      random_id_list = map element_id elements
      random_id_list_with_duplicates = random_id_list ++ [random_id]
      random_id_list_without_duplicates = nub random_id_list_with_duplicates
      random_id_list_without_duplicates_length = length random_id_list_without_duplicates
      random_id_list_without_duplicates_length_minus_one = random_id_list_without_duplicates_length - 1
      random_id_list_without_duplicates_last_element = random_id_list_without_duplicates !! random_id_list_without_duplicates_length_minus_one
   in if random_id_list_without_duplicates_length == random_id_list_without_duplicates_length_minus_one
        then random_id_list_without_duplicates_last_element
        else randomElementId elements

-- create a list of random elements using randomElementId for non-duplicate ids and acording to the env_size
randomElements :: [Element] -> (Int , Int) -> Int -> [Element]
randomElements elements env_size amount =
  let --check amount of elements before creating a new element
      amount_of_elements = length elements
      random_elements = elements
      a =
        if amount_of_elements < amount
          then
            let random_element_type = randomElementType
                random_element_pos = randomPosition env_size
                random_element_id = randomElementId elements
                random_element_skill = False
                random_element = Element {element_pos = Coord {x = fst random_element_pos, y = snd random_element_pos}, element_type = random_element_type, element_id = random_element_id, element_skill = random_element_skill}
                random_elements = random_elements ++ [random_element]
             in randomElements random_elements env_size amount
          else random_elements
   in random_elements


