module Lib
  (buildWordGrid,
   search,
   Grid,
   Node,
   loadGrid,
   saveGrid,
   word,
   links)
where

import qualified Data.List as List
import Data.HashSet (HashSet)
import qualified Data.HashSet as HashSet
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.PQueue.Min (MinQueue)
import qualified Data.PQueue.Min as MinQueue
import Data.Hashable (Hashable, hashWithSalt)
import Data.Text (Text)
import qualified Data.Text as Text

data Grid = Grid (HashMap Text Node)

data Trace = Trace {dis :: Int, cost :: Int, nTrace :: [Node]}
  deriving Show

data Node = Node {
  word :: Text,
  links :: [Node]
  }

instance Eq Node where
  (==) Node {word = a} Node {word = b} = (==) a b

instance Ord Node where
  compare Node{ word = a } Node{ word = b } = compare a b

instance Show Node where
  show = Text.unpack . word

instance Hashable Node where
  hashWithSalt s node = hashWithSalt s (word node)

instance Eq Trace where
  (==) Trace { cost = a } Trace { cost = b } = (==) a b

instance Ord Trace where
  compare Trace { cost = a } Trace { cost = b } = compare a b

errorNode = Node {word = Text.empty, links = []}

wordDistance wordA wordB =
  sum $ map (\(a, b) -> if a /=b then 1 else 0) $ Text.zip wordA wordB

wordsConnected wordA wordB =
  wordDistance wordA wordB == 1

wordLinks bins targetWord =
  filter (\w -> wordsConnected targetWord (word w)) $ bins !! Text.length targetWord

buildWordGrid wordList =
  Grid (HashMap.fromList $ map (\d -> (word d, d)) dic)
  where
    dic = map (\w -> Node {word = w, links = wordLinks dicBins w}) wordList
    dicBins = map (\i -> filter (\w -> Text.length (word w) == i) dic) [0..]

getNode dic name = HashMap.lookup name dic

traceOf target t = Trace { dis = 0, cost = wordDistance (word target) (word t), nTrace = [t] }
traceExtend target node Trace { dis = oldDis, nTrace = trace } =
  Trace {
  dis = newDis,
  cost = newDis + wordDistance (word target) (word node),
  nTrace = node:trace
  }
  where newDis = oldDis + 1

insertAll queue [] = queue
insertAll queue (x:xs) = MinQueue.insert x (insertAll queue xs)

search :: Grid -> Text -> Text -> Maybe [Node]
search (Grid graph) start end =
  if (Text.length start) == (Text.length end)
  then case (startNode, endNode) of
    (Just s, Just e) -> search' (MinQueue.singleton $ traceOf s e) HashSet.empty s
    _ -> Nothing
  else Nothing
  where
    startNode = getNode graph start
    endNode = getNode graph end

search' :: MinQueue Trace -> HashSet Node -> Node -> Maybe [Node]
search' queue seen target
  | MinQueue.null queue = Nothing
  | otherwise =
    if current == target
    then Just $ nTrace $trace
    else search' bQueue aSeen target
  where
    (trace, aQueue) = MinQueue.deleteFindMin queue
    current = head $ nTrace $ trace
    aSeen = HashSet.insert current seen
    connections = filter (\n -> not $ n `HashSet.member` aSeen) (links current)
    bQueue = insertAll aQueue $ map (\n -> (traceExtend target n trace)) connections


saveGrid :: Grid -> [Text]
saveGrid (Grid grid) =
  map saveNode $ filter (\n -> not $ List.null $ links n) nodes
  where
    nodes = map (\(_, v) -> v) $ HashMap.toList grid
    saveNode Node{word = w, links = l} = w `Text.append` (Text.cons ' ' $ Text.unwords $ map (\Node{word = linkW} -> linkW) l)

loadGrid save = Grid grid
  where
    wordsSave = map Text.words save
    nodes = map loadNode wordsSave
    grid = HashMap.fromList $ map (\n -> (word n, n)) nodes

    lookup word = HashMap.lookupDefault errorNode word grid

    loadNode line = Node {word = w, links = l}
      where
        (w:t) = line
        l = map lookup t
