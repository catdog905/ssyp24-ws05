{-# LANGUAGE OverloadedStrings #-}
import CodeWorld
 
-- lettering :: String -> Picture
 
shift :: Double
shift = 1.1
 
data Tile = Grass | Wall | Door Bool | Lake | Duck
  deriving Eq
  
data GameMap = GameMap [[Tile]]
 
class Drawable a where
  draw :: a -> Picture

instance Drawable GameMap where
  draw (GameMap tiles) = drawMap tiles
    where
      drawMap :: [[Tile]] -> Picture
      drawMap [] = blank 
      drawMap (x:xs) = (drawLine x) <> (translated 0 shift (drawMap xs))
      
instance Drawable PlayerPosition where
  draw player = drawPlayer player
    where
      drawPlayer :: PlayerPosition -> Picture
      drawPlayer (PlayerPosition (x, y)) = translated 
                                      ((fromIntegral x)*1.1)
                                      ((fromIntegral y)*1.1)
                                      (colored  red (solidCircle 0.5)) 
replaceBylINE :: [a] -> Int -> a -> [a]
replaceBylINE [] _ _ = []
replaceBylINE (x:xs) 0 obj = (obj: xs)
replaceBylINE (x:xs) n obj = x : (replaceBylINE xs (n -1) obj)

replaceByInd::[[a]] -> Int -> Int -> a -> [[a]]
replaceByInd [] _ _ _ = []
replaceByInd (line:liness) x 0 obj = (replaceBylINE line x obj):liness
replaceByInd (line:liness) x y obj = line:(replaceByInd liness x (y-1) obj)


 
drawTitle :: Tile -> Picture
drawTitle Grass = colored(darker 0.2 green) (solidRectangle 1 1)
drawTitle Wall = colored  grey(solidRectangle 1 1)
drawTitle (Door False) = colored brown (solidRectangle 1 1 )
drawTitle (Door True) = colored (darker 0.2 green) (solidRectangle 1 1 )
drawTitle Duck = colored blue (solidRectangle 1 1)


drawLine :: [Tile] -> Picture
drawLine [] = blank
drawLine (x:xs) = drawTitle x <>
                  (translated shift 0 (drawLine xs))
 
 
data PlayerPosition = PlayerPosition (Int, Int)
 
data World = World PlayerPosition GameMap
 
controller :: Event -> World -> World
controller (KeyPress "W") (World (PlayerPosition (x, y)) (GameMap gameMap))
  | (findByIndexes gameMap x (y + 1)) == Just Wall || (findByIndexes gameMap x (y + 1)) == Just (Door False) = (World (PlayerPosition (x, y)) (GameMap gameMap))
  | otherwise = (World (PlayerPosition (x, y+1)) (GameMap gameMap))
controller (KeyPress "A") (World (PlayerPosition (x, y)) (GameMap gameMap))
  | (findByIndexes gameMap (x-1) y) == Just Wall || (findByIndexes gameMap (x-1)y) == Just (Door False) = (World (PlayerPosition (x, y)) (GameMap gameMap))
  | otherwise = (World (PlayerPosition (x-1, y)) (GameMap gameMap))
controller (KeyPress "S") (World (PlayerPosition (x, y)) (GameMap gameMap))
  | (findByIndexes gameMap x ( y-1)) == Just Wall ||(findByIndexes gameMap x (y-1)) == Just (Door False) = (World (PlayerPosition (x, y)) (GameMap gameMap))
  | otherwise = (World (PlayerPosition (x, y-1)) (GameMap gameMap))
controller (KeyPress "D") (World (PlayerPosition (x, y)) (GameMap gameMap))
  | (findByIndexes gameMap (x+1 )y) == Just Wall || (findByIndexes gameMap (x+1)y) == Just (Door False) = (World (PlayerPosition (x, y)) (GameMap gameMap))
  | otherwise = (World (PlayerPosition (x+1, y)) (GameMap gameMap))
controller (KeyPress "E") (World (PlayerPosition (x, y)) (GameMap gameMap))
  | (findByIndexes gameMap (x+1) y) == Just (Door False) = (World (PlayerPosition (x, y)) (GameMap (replaceByInd gameMap (x+1) y (Door True))))
  | (findByIndexes gameMap (x+1) y) == Just (Door True) = (World (PlayerPosition (x, y)) (GameMap (replaceByInd gameMap (x+1) y (Door False))))
  | (findByIndexes gameMap (x-1) y) == Just (Door False) = (World (PlayerPosition (x, y)) (GameMap (replaceByInd gameMap (x-1) y (Door True))))
  | (findByIndexes gameMap (x-1) y) == Just (Door True) = (World (PlayerPosition (x, y)) (GameMap (replaceByInd gameMap (x-1) y (Door False))))
  | (findByIndexes gameMap (x) (y-1)) == Just (Door False) = (World (PlayerPosition (x, y)) (GameMap (replaceByInd gameMap (x) (y-1) (Door True))))
  | (findByIndexes gameMap (x) (y-1)) == Just (Door True) = (World (PlayerPosition (x, y)) (GameMap (replaceByInd gameMap (x)( y-1) (Door False))))
  | (findByIndexes gameMap (x) (y+1)) == Just (Door False) = (World (PlayerPosition (x, y)) (GameMap (replaceByInd gameMap (x) ( y+1 ) (Door True))))
  | (findByIndexes gameMap (x) (y+1)) == Just (Door True) = (World (PlayerPosition (x, y)) (GameMap (replaceByInd gameMap  (x) ( y+1 ) (Door False))))

  | otherwise = (World (PlayerPosition (x, y)) (GameMap gameMap))
controller _ playerPos = playerPos
 

instance Drawable World where
  draw world = drawWorld world

drawWorld :: World -> Picture
drawWorld (World playerPos gameMap) =
           scaled 0.75 0.75 (
            translated (-(fromIntegral width) / 2) (-(fromIntegral height) / 2) 
              ((draw playerPos) <> (draw gameMap))
              )
  where
    height = case gameMap of
      (GameMap tiles) ->length tiles
    width = case gameMap of
      (GameMap []) -> 0
      (GameMap (x:xs)) -> length x
 
 
 
line :: Int -> a -> [a]
line 0 _ = []
line x obj = obj : (line (x-1) obj)

getElemById :: [a] -> Int -> Maybe a
getElemById [] _ = Nothing
getElemById (x:xs) 0 = Just x
getElemById (x:xs) n = getElemById xs (n - 1)
 
findByIndexes :: [[a]] -> Int -> Int -> Maybe a
findByIndexes [] _ _ = Nothing
findByIndexes (line:liness) i 0 = getElemById line i
findByIndexes (line:liness) i j = findByIndexes liness i (j-1)
 
initialWorld = World 
                 (PlayerPosition (1, 1))
                 (GameMap (
                  [(line 16 Wall)]
                   ++
                  (line 1 ( Wall :( line 2 Grass) ++ ( line 12 Wall ) ++ [Wall ]))
                  ++
                  (line 1 ( Wall :( line 1 Wall)++ (line 1 Grass) ++ (line 3 Wall) ++ (line 7 Grass)++ ( line 2 Wall) ++ [Wall ]))
                  ++
                  (line 1 ( Wall :( line 1 Wall)++(line 1 Grass) ++ (line 4 Grass)++ (line 1 Wall) ++ (line 1 Grass) ++( line 4 Wall) ++ (line 1 Grass) ++( line 1 Wall) ++ [Wall ]))
                  ++
                  (line 1 ( Wall :( line 4 Wall)++ (line 1 Wall) ++ (line 5 Grass) ++ (line 3 Wall)  ++( line 1 Grass) ++ [Wall ]))
                  ++
                 (line 1 ( Wall : (line 5 Grass) ++ (line 2 Wall) ++ (line 5 Grass)  ++(line 2 Grass) ++ [Wall ]))
                  ++
                  (line 1 ( Wall : (line 1 Grass) ++ (line 6 Wall) ++ (line 1 Grass) ++ (line 5 Wall) ++(line 1 Grass) ++ [Wall ]))
                  ++
                  (line 1 ( Wall : (line 8 Grass) ++ (line 5 Wall) ++ ( line 1 ( Door False)) ++[Wall ]))
                  ++
                  (line 1 ( Wall : (line 5 Wall) ++ (line 1 Grass) ++ (line 6 Wall) ++(line 1 Duck) ++ (line 1 Grass) ++ [Wall ]))
                  ++
                  [(line 16 Wall)]
                  ))
 
 
main :: IO ()
main = activityOf 
      initialWorld controller draw
