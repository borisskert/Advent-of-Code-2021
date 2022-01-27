{-# LANGUAGE TupleSections #-}

module Day23 where

-- https://adventofcode.com/2021/day/23

import Data.Graph (Graph, Vertex, dfs, graphFromEdges, vertices)
import Data.List (group, nub, partition)
import Data.Map (Map, delete, elems, empty, fromList, insert, keys, lookup, member, toList, unionWith, unions)
import Data.Maybe (catMaybes, fromJust, isJust, isNothing, mapMaybe)
import Data.Tree (Forest, Tree (rootLabel, subForest), levels)
import Prelude hiding (lookup)

type Energy = Int

type Material = Char

type Amphipods = Map PlaceId Material

type PlaceType = Char

type PlaceId = (PlaceType, Int)

type Place = (Maybe Material)

type Edges = Map PlaceId [PlaceId]

type Field = (Amphipods, Edges)

type VertexGraph = (Graph, Vertex -> (Place, PlaceId, [PlaceId]), PlaceId -> Maybe Vertex)

type Position = (Int, Int)

type Grid = Map Position Material

parseToGrid :: String -> Grid
parseToGrid input =
  fromList
    . concatMap (uncurry toLine)
    . zip [0 ..]
    $ inputLines
  where
    inputLines = lines input :: [[Char]]

    toLine :: Int -> [Char] -> [(Position, Char)]
    toLine y line = zipWith (\x c -> ((x, y), c)) [0 ..] line

parseInput :: String -> Amphipods
parseInput =
  fromList
    . mapMaybe toAmphipod
    . filter ((`elem` rooms) . snd)
    . toList
    . parseToGrid
  where
    toAmphipod :: (Position, Material) -> Maybe (PlaceId, Material)
    toAmphipod (position, material)
      | y == 1 = Just (('H', x - 1), material)
      | x == 3 = Just (('A', y - 2), material)
      | x == 5 = Just (('B', y - 2), material)
      | x == 7 = Just (('C', y - 2), material)
      | x == 9 = Just (('D', y - 2), material)
      | otherwise = Nothing
      where
        (x, y) = position

rooms = ['A', 'B', 'C', 'D'] :: [Material]

energyOf :: Material -> Energy
energyOf 'A' = 1
energyOf 'B' = 10
energyOf 'C' = 100
energyOf 'D' = 1000
energyOf _ = error "Unknown material"

placeIdFromNode :: (Place, PlaceId, [PlaceId]) -> PlaceId
placeIdFromNode (_, placeId, _) = placeId

isHallway :: PlaceId -> Bool
isHallway (type', _) = type' == 'H'

roomOf :: PlaceId -> Material
roomOf (type', _) = type'

isRoom :: PlaceId -> Bool
isRoom (type', _) = type' `elem` rooms

isBehind :: Edges -> PlaceId -> Bool
isBehind edges placeId
  | type' `elem` rooms = all ((== type') . fst) . fromJust . lookup placeId $ edges
  | otherwise = number == 0 || number == 10
  where
    (type', number) = placeId

isAhead :: Edges -> PlaceId -> Bool
isAhead edges placeId
  | type' `elem` rooms = any ((/= type') . fst) . fromJust . lookup placeId $ edges
  | otherwise = number == 1 || number == 9
  where
    (type', number) = placeId

behindPlaceOf :: Edges -> PlaceId -> PlaceId
behindPlaceOf edges placeId
  | not . isAhead edges $ placeId = error "Not a ahead place!"
  | isHallway placeId && number == 1 = ('H', 0)
  | isHallway placeId = ('H', 10)
  | otherwise = (type', 1)
  where
    (type', number) = placeId

aheadPlaceOf :: Edges -> PlaceId -> PlaceId
aheadPlaceOf edges placeId
  | not . isBehind edges $ placeId = error "Not a behind place!"
  | isHallway placeId && number == 0 = ('H', 1)
  | isHallway placeId = ('H', 9)
  | otherwise = (type', 0)
  where
    (type', number) = placeId

isReserved :: Amphipods -> PlaceId -> Bool
isReserved amphipods placeId = (placeId `elem`) . keys $ amphipods

isForbidden :: Field -> PlaceId -> Bool
isForbidden field placeId
  | isRoom placeId && isBehind edges placeId = False
  | isRoom placeId && (not . isReserved amphipods $ behindPlace) = True
  | otherwise = placeId `elem` generallyForbidden
  where
    (amphipods, edges) = field
    behindPlace = behindPlaceOf edges placeId :: PlaceId

    generallyForbidden =
      [ ('H', 2),
        ('H', 4),
        ('H', 6),
        ('H', 8)
      ]

isInTarget :: PlaceId -> Material -> Bool
isInTarget (room, _) material = room == material

isFinished :: PlaceId -> Field -> Bool
isFinished placeId field
  | isNothing place = False
  | not . isInTarget placeId $ amphipod = False
  | isBehind edges placeId = True
  | isJust behindPlace = behindAmphipod == amphipod
  | otherwise = False
  where
    (amphipods, edges) = field
    place = lookup placeId amphipods :: Place
    amphipod = fromJust place :: Material
    behindPlaceId = behindPlaceOf edges placeId :: PlaceId
    behindPlace = lookup behindPlaceId amphipods :: Place
    behindAmphipod = fromJust behindPlace :: Material

isLegal :: Field -> Move -> Bool
isLegal field move
  | isFinished sourcePlaceId field = False
  | isReserved amphipods targetPlaceId = False
  | isRoom targetPlaceId = roomOf targetPlaceId == material && targetWillNotBlock
  | isRoom sourcePlaceId && isRoom targetPlaceId = roomOf sourcePlaceId /= roomOf targetPlaceId
  | isHallway sourcePlaceId = not . isHallway $ targetPlaceId
  | otherwise = not . isForbidden field $ targetPlaceId
  where
    (material, sourcePlaceId, targetPlaceId) = move
    (amphipods, edges) = field

    targetWillNotBlock :: Bool
    targetWillNotBlock
      | isBehind edges targetPlaceId = True
      | isNothing behindPlace = False
      | otherwise = behindAmphipod == material
      where
        behindPlaceId = behindPlaceOf edges targetPlaceId :: PlaceId
        behindPlace = lookup behindPlaceId amphipods :: Place
        behindAmphipod = fromJust behindPlace

createEdgesList :: Field -> [(Place, PlaceId, [PlaceId])]
createEdgesList field = map toEdge locations
  where
    (amphipods, edges) = field
    locations = keys edges :: [PlaceId]

    toEdge :: PlaceId -> (Place, PlaceId, [PlaceId])
    toEdge placeId
      | member placeId amphipods = (place, placeId, placeEdges)
      | otherwise = (Nothing, placeId, placeEdges)
      where
        placeEdges = fromJust . lookup placeId $ edges :: [PlaceId]
        place = lookup placeId amphipods :: Place

createGraph :: Field -> VertexGraph
createGraph field = graphFromEdges edgesList
  where
    edgesList = createEdgesList field :: [(Place, PlaceId, [PlaceId])]

searchPath :: (Eq a) => a -> Tree a -> Maybe [a]
searchPath val tree
  | val == root = Just [val]
  | null foundInSubs = Nothing
  | otherwise = Just (root : foundPath)
  where
    root = rootLabel tree
    subs = subForest tree
    foundInSubs = mapMaybe (searchPath val) subs
    foundPath = head foundInSubs

pathOf :: Move -> Field -> [PlaceId]
pathOf move amphipods = map (placeIdFromNode . nodeFromVertex) . fromJust . searchPath targetVertex $ sourceTree
  where
    (amphipod, sourcePlaceId, targetPlaceId) = move
    (graph, nodeFromVertex, vertexFromKey) = createGraph amphipods
    sourceVertex = fromJust . vertexFromKey $ sourcePlaceId :: Vertex
    targetVertex = fromJust . vertexFromKey $ targetPlaceId :: Vertex
    sourceTree = head . dfs graph $ [sourceVertex] :: Tree Vertex

possibleTargets :: PlaceId -> Field -> [PlaceId]
possibleTargets placeId field = searchTargetsFor . head . dfs graph $ [vertex]
  where
    (amphipods, edges) = field
    (graph, nodeFromVertex, vertexFromKey) = createGraph field
    vertex = fromJust . vertexFromKey $ placeId :: Vertex

    searchTargetsFor :: Tree Vertex -> [PlaceId]
    searchTargetsFor vertexNode = concatMap searchTargets subs
      where
        (place, placeId, _) = nodeFromVertex . rootLabel $ vertexNode :: (Place, PlaceId, [PlaceId])
        subs = subForest vertexNode :: Forest Vertex

    searchTargets :: Tree Vertex -> [PlaceId]
    searchTargets vertexNode
      | isReserved amphipods placeId = []
      | isForbidden field placeId = concatMap searchTargets subs
      | otherwise = (placeId :) . concatMap searchTargets $ subs
      where
        (place, placeId, _) = nodeFromVertex . rootLabel $ vertexNode :: (Place, PlaceId, [PlaceId])
        subs = subForest vertexNode :: Forest Vertex

type Move = (Material, PlaceId, PlaceId)

possibleMovesOf :: PlaceId -> Field -> [Move]
possibleMovesOf placeId field = filter (isLegal field) . map (amphipod,placeId,) $ targets
  where
    (amphipods, edges) = field
    targets = possibleTargets placeId field :: [PlaceId]
    amphipod = fromJust . lookup placeId $ amphipods :: Material

movesIntoTarget :: Move -> Bool
movesIntoTarget move = roomOf target == material
  where
    (material, source, target) = move

nextMovesOf :: PlaceId -> Field -> [Move]
nextMovesOf placeId field
  | any movesIntoTarget possible = (: []) . head . filter movesIntoTarget $ possible
  | otherwise = possible
  where
    possible = possibleMovesOf placeId field :: [Move]

nextMoves :: Field -> [Move]
nextMoves field = nub . concatMap (`nextMovesOf` field) $ placeIds
  where
    (amphipods, edges) = field
    placeIds = keys amphipods :: [PlaceId]

performMove :: Move -> Field -> Field
performMove move field =
  (,edges)
    . insert targetPlaceId material
    . delete sourcePlaceId
    $ amphopids
  where
    (amphopids, edges) = field
    (material, sourcePlaceId, targetPlaceId) = move

hasBeenCompleted :: Field -> Bool
hasBeenCompleted field = all (`isFinished` field) . keys $ amphipods
  where
    (amphipods, edges) = field

isDead :: Field -> Bool
isDead = null . nextMoves

costs :: Move -> Field -> Energy
costs move amphipods = (* energy) count
  where
    (material, sourcePlaceId, targetPlaceId) = move
    path = pathOf move amphipods :: [PlaceId]
    count = length . tail $ path :: Energy
    energy = energyOf material :: Energy

type Solution = (Amphipods, Energy)

--                       open                visited                complete         dead
type Solutions = (Map Amphipods Energy, Map Amphipods Energy, Maybe Energy, Map Amphipods Energy)

next :: Edges -> Solution -> [Solution]
next edges solution = map go moves
  where
    (amphipods, costs') = solution
    field = (amphipods, edges)
    moves = nextMoves (amphipods, edges) :: [Move]

    go :: Move -> Solution
    go move = (fst . performMove move $ field, costs' + costs move field)

search :: Field -> Energy
search field = searchRecursively edges (initial, empty, Nothing, empty)
  where
    (amphipods, edges) = field
    initial = insert amphipods 0 empty

searchRecursively :: Edges -> Solutions -> Energy
searchRecursively edges (currents, visited, completed, dead)
  | null currents = fromJust completed
  | otherwise = searchRecursively edges (nextCurrent, nextVisited, nextCompleted, nextDead)
  where
    current = head . toList $ currents
    (amphipods, costs) = current
    nextSolutions = next edges current :: [Solution]
    (visited', notVisited) = partition ((`member` visited) . fst) nextSolutions
    (completed', notCompleted) = partition (hasBeenCompleted . (,edges) . fst) notVisited
    (dead', open) = partition (isDead . (,edges) . fst) notCompleted

    openMap = fromList open
    visitedButFaster = fromList . filter isVisitedButFaster $ visited'

    nextCurrent = unionWith takeMinimum visitedButFaster . unionWith takeMinimum openMap . delete amphipods $ currents

    nextVisited = (deleteAll . keys $ visitedButFaster) . insert amphipods costs $ visited

    nextCompleted
      | null completed' = completed
      | isNothing completed && null completed' = Nothing
      | isNothing completed = Just min
      | otherwise = Just . minimum $ [min, fromJust completed]
      where
        min = minimum . map snd $ completed'

    nextDead = unionWith takeMinimum dead . fromList $ dead'

    isVisitedButFaster :: Solution -> Bool
    isVisitedButFaster (a, newCosts) = oldCosts > newCosts
      where
        oldCosts = fromJust . lookup a $ visited

takeMinimum :: Energy -> Energy -> Energy
takeMinimum a b = minimum [a, b]

deleteAll :: (Eq a, Ord a) => [a] -> Map a b -> Map a b
deleteAll keys m = foldl (flip delete) m keys

leastEnergyRequired :: String -> Energy
leastEnergyRequired = search . (,edges) . parseInput

edges =
  fromList
    [ (('H', 0), [('H', 1)]),
      (('H', 1), [('H', 0), ('H', 2)]),
      (('H', 2), [('H', 1), ('H', 3), ('A', 0)]),
      (('H', 3), [('H', 2), ('H', 4)]),
      (('H', 4), [('H', 3), ('H', 5), ('B', 0)]),
      (('H', 5), [('H', 4), ('H', 6)]),
      (('H', 6), [('H', 5), ('H', 7), ('C', 0)]),
      (('H', 7), [('H', 6), ('H', 8)]),
      (('H', 8), [('H', 7), ('H', 9), ('D', 0)]),
      (('H', 9), [('H', 8), ('H', 10)]),
      (('H', 10), [('H', 9)]),
      (('A', 0), [('A', 1), ('H', 2)]),
      (('A', 1), [('A', 0)]),
      (('B', 0), [('B', 1), ('H', 4)]),
      (('B', 1), [('B', 0)]),
      (('C', 0), [('C', 1), ('H', 6)]),
      (('C', 1), [('C', 0)]),
      (('D', 0), [('D', 1), ('H', 8)]),
      (('D', 1), [('D', 0)])
    ] ::
    Edges
