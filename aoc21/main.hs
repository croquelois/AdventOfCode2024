import System.IO  
import Data.List.Split
import Data.Ratio
import qualified Data.List as List
import qualified Data.Set as Set
import qualified Data.Map as Map
import Control.DeepSeq
import System.CPUTime
import Text.Printf
import Control.Monad
import Control.Exception
import Debug.Trace
import Data.Maybe

-- Types and constants

-- Computation
 
-- Main

q1 filename = do 
  content <- readFile filename
  let codes = lines content
  print $ codes

main = do 
  q1 "test1.txt"
  q1 "data.txt"
