module Main where

import DataTypes
import CommonFunctions
import Debug.Trace

{-

Marco General

El ambiente en el cual intervienen los agentes es discreto y tiene la forma de
un rectángulo de N × M . El ambiente es de información completa, por tanto
todos los agentes conocen toda la información sobre el agente. El ambiente puede
varı́ar aleatoriamente cada t unidades de tiempo. El valor de t es conocido.
Las acciones que realizan los agentes ocurren por turnos. En un turno, los
agentes realizan sus acciones, una sola por cada agente, y modifican el medio
sin que este varı́e a no ser que cambie por una acción de los agentes. En el
siguiente, el ambiente puede variar.Si es el momento de cambio del ambiente,
ocurre primero el cambio natural del ambiente y luego la variación aleatoria.
En una unidad de tiempo ocurren el turno del agente y el turno de cambio del
ambiente.
Los elementos que pueden existir en el ambiente son obstáculos, suciedad,
niños, el corral y los agentes que son llamados Robots de Casa. A continuación
se precisan las caracterı́sticas de los elementos del ambiente:
Obstáculos: estos ocupan una única casilla en el ambiente. Ellos pueden ser
movidos, empujándolos, por los niños, una única casilla. El Robot de Casa
sin embargo no puede moverlo. No pueden ser movidos ninguna de las
casillas ocupadas por cualquier otro elemento del ambiente.
Suciedad: la suciedad es por cada casilla del ambiente. Solo puede aparecer
en casillas que previamente estuvieron vacı́as. Esta, o aparece en el estado
inicial o es creada por los niños.
Corral: el corral ocupa casillas adyacentes en número igual al del total de niños
presentes en el ambiente. El corral no puede moverse. En una casilla del
corral solo puede coexistir un niño. En una casilla del corral, que esté vacı́a,
puede entrar un robot. En una misma casilla del corral pueden coexistir
un niño y un robot solo si el robot lo carga, o si acaba de dejar al niño.
Niño: los niños ocupan solo una casilla. Ellos en el turno del ambiente se mue-
ven, si es posible (si la casilla no está ocupada: no tiene suciedad, no está el
corral, no hay un Robot de Casa), y aleatoriamente (puede que no ocurra
movimiento), a una de las casilla adyacentes. Si esa casilla está ocupada
por un obstáculo este es empujado por el niño, si en la dirección hay más
de un obstáculo, entonces se desplazan todos. Si el obstáculo está en una
posición donde no puede ser empujado y el niño lo intenta, entonces el
obstáculo no se mueve y el niño ocupa la misma posición.
Los niños son los responsables de que aparezla suciedad. Si en una cuadrı́cu-
la de 3 por 3 hay un solo niño, entonces, luego de que él se mueva aleato-
riamente, una de las casillas de la cuadrı́cula anterior que esté vacı́a puede
haber sido ensuciada. Si hay dos niños se pueden ensuciar hasta 3. Si hay
tres niños o más pueden resultar sucias hasta 6.
Los niños cuando están en una casilla del corral, ni se mueven ni ensucian.
Si un niño es capturado por un Robot de Casa tampoco se mueve ni
ensucia.
Robot de Casa: El Robot de Casa se encarga de limpiar y de controlar a
los niños. El Robot se mueve a una de las casillas adyacentee, las que
decida. Solo se mueve una casilla sino carga un niño. Si carga un niño
pude moverse hasta dos casillas consecutivas.
También puede realizar las acciones de limpiar y cargar niños. Si se mueve
a una casilla con suciedad, en el próximo turno puede decidir limpiar o
moverse. Si se mueve a una casilla donde está un niño, inmediatamente lo
carga. En ese momento, coexisten en la casilla Robot y niño.
Si se mueve a una casilla del corral que está vacı́a, y carga un niño, puede
decidir si lo deja esta casilla o se sigue moviendo. El Robot puede dejar al
2niño que carga en cualquier casilla. En ese momento cesa el movimiento
del Robot en el turno, y coexisten hasta el próximo turno, en la misma
casilla, Robot y niño.


Objetivos
El objetivo del Robot de Casa es mantener la casa limpia. Se considera la
casa limpia si el 60 % de las casillas vacias no están sucias.

-}



-- Main Function here
main :: IO ()
main = do
  -- start simulation process
  putStrLn "Starting Simulation"
  -- start simulation
  let 
    final = startSimulation initEnv
  in 
    putStrLn $ show final
  -- end simulation
  putStrLn "Simulation Ended"


-- Start Simulation 
startSimulation :: Env -> Env
startSimulation env = do
  -- get next env
  nextEnv <- getNextEnv env
  -- check if simulation is over
  if isSimulationOver nextEnv
    then nextEnv
    else
      -- print env
      (trace (show nextEnv)) $ startSimulation nextEnv
      

-- Is Over if the total number of clean cells is 60% of the total number of cells
isSimulationOver :: Env -> Bool
-- integer to float conversion
isSimulationOver env = fromIntegral (totalCleanCells env) / fromIntegral (totalCells env) > 0.6



totalCells env =
  let
    (x,y) = env_size env
  in
    x * y


-- total clean tiles is equal to total of elements wich type is Dirt - total of tiles
totalCleanCells env =
  let
    (x,y) = env_size env
    totalCells = x * y
    -- count total of dirt type elements in env
    dirtCount = length $ filter (\e -> element_type e == Dirt) $ env_elements env

  in
    totalCells - dirtCount


-- for all elements possible actions in order check if it is possible to do it
-- if it is possible, then execute the action passing the element and the env
-- if not, dont do anything
-- after the loop, its the env turn
-- roll a random number and if it is less than env_random_change , then change the env
getNextEnv :: Env -> Env
getNextEnv env =
  let
    elements = env_elements env
    nextEnv = foldl doElementsActions env elements

  in
    if env_random_change env > 0
      then if (randomInt 0 random_max) < (env_random_change env)
        then changeEnv nextEnv
        else nextEnv
      else nextEnv
  

doElementsActions :: Env -> Element -> Env
doElementsActions = error "doElementsActions not implemented"

changeEnv :: Env -> Env
changeEnv = error "changeEnv not implemented"




initEnv = let env = Env {
  env_size = (5,5), --(randomInt 3 10 , randomInt 3 10),x, y)
  env_turn = 0,
  env_random_change = 5,
  env_elements = [
    Element {
      element_pos = Coord {x = 2, y = 3},
      element_type = Baby,
      element_id = 0,
      element_skill = False
    },
    Element {
      element_pos = Coord {x = 4, y = 1},
      element_type = Dirt,
      element_id = 1,
      element_skill = False
    },
    Element {
      element_pos = Coord {x = 3, y = 1},
      element_type = Dirt,
      element_id = 5,
      element_skill = False
    },
    Element {
      element_pos = Coord {x = 2, y = 1},
      element_type = Dirt,
      element_id = 6,
      element_skill = False
    },
    Element {
      element_pos = Coord {x = 4, y = 2},
      element_type = Dirt,
      element_id = 7,
      element_skill = False
    },
    Element {
      element_pos = Coord {x = 1, y = 2},
      element_type = Robot,
      element_id = 2,
      element_skill = False
    },
    Element {
      element_pos = Coord {x = 3, y = 1},
      element_type = PlayPen,
      element_id = 3,
      element_skill = False
    },
    Element {
      element_pos = Coord {x = 3, y = 4},
      element_type = Obstacle,
      element_id = 4,
      element_skill = False
    }
  ] --randomElementsCreator env_size

}
  in if check_consistency env then env else error "inconsistent environment"


check_consistency env = True

-- check if the elements are consistent
  -- Robot are not allowed to be in the same position as a Baby
  -- Obstacles are not allowed to be in the same position of any other element
  -- PlayPens are not allowed to be in the same position of another PlayPen
  -- PlayPens must be adjacent to each other(horizontal or vertical)


--check_consistency_elements env =











