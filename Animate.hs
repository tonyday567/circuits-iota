-- | Animate an iota tree: progressive level-by-level reveal.
--
-- Takes an iota 0/1 string, builds a binary tree, computes radial
-- positions, and renders a frame per level.
module Main where

import Chart
import Optics.Core
import Data.List (foldl')
import qualified Data.Text as T

-- * Tree from iota string

data ITree = Leaf | Node ITree ITree deriving (Show, Eq)

-- Parse "1" = Leaf, "0 a b" = Node a b
parseIota :: String -> (ITree, String)
parseIota ('1' : cs) = (Leaf, cs)
parseIota ('0' : cs) =
  let (l, cs1) = parseIota cs
      (r, cs2) = parseIota cs1
  in (Node l r, cs2)
parseIota _ = error "bad iota"

-- Breadth-first levels
levels :: ITree -> [[ITree]]
levels t = go [t]
  where
    go [] = []
    go xs = xs : go [c | Node l r <- xs, c <- [l, r]]

-- * Radial layout

type Pt = Point Double

radialLayout :: ITree -> [(Pt, Pt, ITree)]  -- (from, to, node)
radialLayout root =
  let lvls = levels root
      maxDepth = length lvls - 1
      positioned = flip map [0 .. maxDepth] $ \d ->
        let nodes = lvls !! d
            n = length nodes
            radius = fromIntegral d / fromIntegral (maxDepth + 1)
        in flip map (zip [0..] nodes) $ \(i, nd) ->
          let angle = if n <= 1 then 0 else 2 * pi * fromIntegral i / fromIntegral n
              pos = Point (radius * cos angle) (radius * sin angle)
          in (pos, nd)
      -- Link each node to its children
      children (Node l r) = [l, r]
      children Leaf = []
      linked = concat $ zipWith (\depthIx ps ->
        let cs = concatMap (\(_, nd) -> children nd) ps
            cs' = snd (positioned !! (depthIx + 1))
            findPos t = fst $ head $ filter (\(_, nd) -> nd == t) cs'
        in [(p, findPos c, c) | (p, nd) <- ps, c <- children nd])
        [0 .. maxDepth - 1] positioned
  in linked

-- * Chart rendering

radius :: Double
radius = 0.9  -- normalised extent

pal :: Int -> Colour
pal = palette

edgeStyle :: Int -> Int -> Style
edgeStyle maxD d =
  defaultPathStyle
    & set #color (pal d `withOpacity` 0.6)
    & set #size (1.5e-3 * fromIntegral (maxD - d + 1))

nodeStyle :: Int -> Style
nodeStyle d =
  defaultGlyphStyle
    & set #color (pal d `withOpacity` 0.9)
    & set #size (8e-3)
    & set #glyphShape CircleGlyph

frameStyle :: Int -> [(Pt, Pt, ITree)] -> ChartOptions
frameStyle maxDepth edges =
  let edgeCharts =
        [ PathChart (edgeStyle maxDepth d)
            [StartP from, LineP to]
        | (from, to, _) <- edges
        ]
      nodePositions = nubPoints $ concat [[from, to] | (from, to, _) <- edges]
      nodeCharts =
        [ GlyphChart (nodeStyle 0) [p]
        | p <- nodePositions
        ]
      nubPoints = map head . group . sort
      group = foldl' (\acc x -> case acc of
        (g@(y:_):gs) | norm (x - y) < 1e-9 -> (x:g):gs
        _ -> [x]:acc) []
      sort = foldl' (\acc x -> insert acc x) []
      insert [] x = [[x]]
      insert (g@(y:_):gs) x | norm (x - y) < 1e-9 = (x:g):gs
                             | otherwise = g : insert gs x
      norm (Point x y) = x*x + y*y
  in mempty
    & set #hudOptions mempty
    & set #chartTree (unnamed $ edgeCharts ++ nodeCharts)

-- Build all frames: progressive reveal by depth
frames :: ITree -> [ChartOptions]
frames root =
  let allEdges = radialLayout root
      maxDepth = maximum [d | (Point _ _, _, _) <- allEdges, let d = floor (norm (Point 0 0))] -- rough
  in allEdges  -- TODO: progressive

main :: IO ()
main = do
  iotaStr <- readFile "le.iota"
  let (tree, _) = parseIota (filter (`elem` "01") iotaStr)
      fs = take 10 (frames tree)
  mapM_ (\(i, f) -> writeChartOptions ("frame" <> show i <> ".svg") f) (zip [0..] fs)
  putStrLn $ "wrote " <> show (length fs) <> " frames"
