{-# LANGUAGE OverloadedStrings #-}
import CodeWorld
import qualified Data.Text as T
import System.Random
import CodeWorld.Image 
 
siteRad = pi / 3
n = 120 
 
sCircle :: Picture
sCircle = image8
 
data Player = Player {
    x :: Double, 
    y :: Double,
    u :: Double,
    boo :: Bool,
    r :: Double,
    count :: Int
  }
 
  deriving (Show)
 
replaceWall :: Wall -> Wall
replaceWall (MoveWall x y size holePos ud)
  | ud == False && y < 3 = MoveWall x (y + 0.05) size (holePos + 0.05) ud
  | ud == False && y >= 3 = MoveWall x (y - 0.05) size (holePos - 0.05) True
  | ud == True && y > (-3) = MoveWall x (y - 0.05) size (holePos - 0.05) ud
  | ud == True && y <= (-3) = MoveWall x (y + 0.05) size (holePos + 0.05) False
replaceWall (Wall x y size holePos) = Wall x y size holePos
 
wallController :: [Wall] -> [Wall]
wallController (Wall x y size holePos:xs) = (Wall x y size holePos):(wallController xs)
wallController (moveWall@(MoveWall _ _ _ _ _):xs) = ((replaceWall moveWall):(wallController  xs))
wallController world = world  
 
replaceRaund :: Player -> Player
replaceRaund (Player x y u boo r count) = (Player x y u boo (u * 20) count)  
 
genericController :: Event -> World -> World
genericController (TimePassing time) (GameOver n) = GameOver (trace (T.pack (show n)) (n + time))
genericController ev world = case newWorld of
  (World player walls) -> World (replaceRaund player) ((wallController (take 5 walls )) ++ (drop 5 walls))
  GameOver n -> newWorld
  where
    newWorld = (controller ev world) 
 
data Wall = Wall {
      xPosition :: Double,
      yPos :: Double,
      holeSize :: Double,
      holePos :: Double
    } 
  |
    MoveWall {
      xPosition :: Double,
      yPos :: Double,
      holeSize :: Double,
      holePos :: Double,
      ud :: Bool
    }
 
  deriving (Show)
 
 
data World = 
    World {
      playerPos :: Player,
      walls :: [Wall]
    } 
  | 
    GameOver Double
 
  deriving (Show)
 
 
normalezed :: Double -> Double -> (Double -> Double)
normalezed myMax myMin = (\x -> ((x - myMin)/(myMax-myMin)-0.5)*20)
 
wall :: Double -> Wall
wall x = Wall 0 x 25 0
 
infWalls :: Double -> [Wall]
infWalls shift = (wall 0) : (map (\wall@(Wall {xPosition=xPos}) -> wall {xPosition = xPos + xPos}) (infWalls shift))
 
calcHypotenuse :: Double -> Double -> Double
calcHypotenuse a b = sqrt (a^2 + b^2) 
 
getElemById :: [a] -> Int -> Maybe a
getElemById [] _ = Nothing
getElemById (x:xs) 0 = Just x
getElemById (x:xs) n = getElemById xs (n - 1) 
 
justa :: Maybe Double -> Double
justa (Just a) = a
justa Nothing = 0 
 
drawLiness :: [(Double, Double)] -> Double -> Double -> (Double -> Double) -> Picture
drawLiness [] _ _ _ = blank
drawLiness ((x, pos):xs) h y0 normaliz = (translated 
                                           0 
                                           (normaliz (pos - y0)) 
                                           (colored (darker (h / 4) green) 
                                             (solidRectangle (x*50) (h*x * 18))) 
                                         <> (drawLiness xs h y0 normaliz))
render :: World -> Picture
render w@(World {walls=(wall:wallss), playerPos=playerPos}) = 
  drawLiness rects (siteSurf / n) y0 (normalezed (y2 - y0) (y1 - y0)) 
  <> (colored (darker 0.3 green) (translated 0 ((normalezed (y2 - y0) (y1 - y0)) yHoleYUp) (solidPolygon [((-2*hipo1), -0.5*hipo1), ((-2*hipo1), 0), ((-1.75*hipo1), 0.2*hipo1*cof1), ((1.75*hipo1), 0.2*hipo1*cof1), ((2*hipo1), 0), ((2*hipo1), -0.5*hipo1)])))
  <> (colored (darker 0.3 green) (translated 0 (((normalezed (y2 - y0) (y1 - y0)) yHoleYDown) + 0.5) (solidPolygon [((-2*hipo2), 0), ((-2*hipo2), -0.5*hipo2), ((-1.75*hipo2), 0.2*hipo2*cof2), ((1.75*hipo2), 0.2*hipo2*cof2), ((2*hipo2), -0.5*hipo2) ,((2*hipo2), 0)])))
  <> translated ((xPosition wall) - x playerPos) 0 (render w{walls = wallss})
  where
    segments :: [Double]
    segments = splitWalls y2 y1 (siteSurf / n) 
    hipos = filter (\(segment) -> (myF wall segment)) hypotenuses
    hipo1 = (justa (getElemById hipos  0)) * 12.5
    hipo2 = (justa (getElemById hipos (length hipos - 1))) * 12.5
    y0 = (y playerPos) 
    y1 = (y0 + (distanceFromPlayerProj * (tan (siteRad / 2))))
    y2 = y0 - (distanceFromPlayerProj * (tan (siteRad / 2)))
    siteSurf = y1 - y2
    cof1 = yHoleYUp - y0
    cof2 = yHoleYDown - y0
    distanceFromPlayerProj = (xPosition wall) - (x playerPos)
    hypotenuses = map (\segment -> 1 / (calcHypotenuse distanceFromPlayerProj ((abs (segment - y0)) / 3))) segments
    yHoleYUp = ((holePos wall + (holeSize wall)/2) - y0)
    yHoleYDown = ((holePos wall - (holeSize wall)/2) - y0)
    rects =  filter (\(_, segment) -> not (myF wall segment)) 
      (zip hypotenuses segments)
render w@(World {walls = []}) = blank
render world = blank
 
myF :: Wall -> Double -> Bool
myF (Wall {holeSize=holeSize, holePos=holePos}) x = ((abs (holePos - x)) <= (holeSize/2))
myF (MoveWall {holeSize=holeSize, holePos=holePos}) x = ((abs (holePos - x)) <= (holeSize/2))
 
find :: (a -> b -> Bool) -> [b] -> a  -> b
find func (m:mx) a
  |  func a m == True = m
  |  func a m == False = m
  |  otherwise = m
 
splitWalls :: Double -> Double -> Double -> [Double]
splitWalls myMin myMax step 
  |  myMin >= myMax = []
  |  otherwise  = myMax : splitWalls myMin (myMax-step) step 
 
ricrolled :: [String]
ricrolled = ["0", "1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12",
             "13", "14", "15", "16", "17", "18", "19", "20", "21", "22", "23", 
             "24", "25", "26", "27", "28", "29", "30", "31"]
 
image2 = image "SDnuhoy" "https://gas-kvas.com/grafic/uploads/posts/2024-01/gas-kvas-com-p-nadpis-s-dnem-rozhdeniya-v-stile-mainkraft-11.jpg" 20 20
image11 = image "dyadyaBanan" "https://celes.club/uploads/posts/2023-03/1679551435_celes-club-p-mish-i-banan-vkontakte-99.jpg" 20 20
image1 = image "ricroll" "https://bflightshows.com/cdn/shop/products/RickRoll_1200x1200.png?v=1648576994" 20 20
image5 = image "beluga1" "https://koshka.top/uploads/posts/2021-11/1637937702_1-koshka-top-p-kot-s-glazami-na-ushakh-1.jpg" 20 20
image7 = image "beluga2" "https://i.pinimg.com/736x/08/a8/c8/08a8c8e136f5bd33c7cec4a0a8384f1e.jpg" 20 20
image6 = image "poshalko" "https://sun6-23.userapi.com/impg/HS1f0Q8pV8WsbePI3jauIZqjDPv606ZXE_B1dQ/ucfai2G9wyI.jpg?size=1080x725&quality=95&sign=c16e13cd57094849e21c8d1c55fef829&c_uniq_tag=3y5GIEwiTvApcqUT521I7MnnJDFjEdh1StHZ1z87eY8&type=album" 20 20
image3 = image "sigma" "https://pic.rutubelist.ru/video/c5/ba/c5ba803eb1af1f1c5e71053006f30242.jpg" 20 20
image8 = image "capybara" "https://baldezh.top/uploads/posts/2023-12/1703264756_baldezh-top-p-kapibara-na-zastavku-pinterest-49.jpg" 1.6 1.6 
image13 = image "flappyBird" "https://www.spriters-resource.com/resources/sheets/57/59894.png?updated=1460962370" 20 20 
image10 = image "diadiaSvin1" "https://encrypted-tbn0.gstatic.com/images?q=tbn:ANd9GcRCr2kGFslaJ1QzU1Ol_p9g8qUpcWAvuy-m4Q&s" 20 20
image4 = image "diadiaSvin2" "https://www.google.com/url?sa=i&url=https%3A%2F%2Fwww.meme-arsenal.com%2Fcreate%2Ftemplate%2F11343271&psig=AOvVaw3lvmWkDVLdayAUPt_1ug57&ust=1721452662665000&source=images&cd=vfe&opi=89978449&ved=0CA8QjRxqFwoTCPCUmoWtsocDFQAAAAAdAAAAABAc" 20 20
image52 = image "Timoha" "https://avatars.mds.yandex.net/i?id=2a00000190b714128b901a659336b393f0ca-1031741-fast-images&n=13" 20 20
 
replacePlayer :: Player -> Player
replacePlayer (Player x y u True r count)
  | u < 0.4 = (Player x (y + u) 0.5 True r count) 
  | u >= 0.4 = (Player x (y + u) u False r count) 
replacePlayer (Player x y u False r count)
  | u > (-0.3) = (Player x (y + u) (u - 0.03) False r count) 
  | u <= (-0.3) = (Player x (y + u) u False r count)  
 
sigmaFun :: Double -> Double -> Double -> Double -> Double -> Bool
sigmaFun a b size x y = (a + 0.2 <= x && a + 2 > x) && ((b <= y - size / 2 || b >= y + size / 2) || b - 0.4 <= y - size / 2 || b + 0.4 >= y + size / 2)  
 
isColision :: World -> Bool
isColision (World (Player b w u boo r count) []) = False
isColision (World (Player b w u boo r count) ((Wall x y size holePos):xs))
  | (sigmaFun 0 w size x y) || (isColision (World (Player b w u boo r count) (take 10 xs))) = True
  | otherwise = False
isColision (World (Player b w u boo r count) ((MoveWall x y size holePos ud:xs)))
  | (sigmaFun 0 w size x y) || (isColision (World (Player b w u boo r count) (take 10 xs))) = True
  | otherwise = False
 
die :: Int -> Bool
die x
  | x == 1488 = False
  | otherwise = True
 
dieMode :: World -> Double -> Bool
dieMode world y 
  | y < -9 = die 52
  | y > 9 = die 52
  | isColision world == True = die 52 
  | otherwise = False 
 
randomName :: Int -> Picture
randomName x = image (T.pack (show x)) (T.pack ("https://raw.githubusercontent.com/catdog905/rickroll/main/static-assets-upload3618021398376013524-" ++ (show x) ++ ".png")) 20 20 
 
playerController :: Event -> Player -> Player
playerController (PointerPress _) (Player x y u b r count) = replacePlayer (trace (T.pack (show count)) (Player x y u True r (count +  1)) )
playerController (KeyPress "Up") (Player x y u b r count) = replacePlayer (trace (T.pack (show count)) (Player x y u True r (count +  1)) )
playerController (KeyPress "W") (Player x y u b r count) = replacePlayer (trace (T.pack (show count)) (Player x y u True r (count +  1)) )
playerController (KeyPress "F") (Player x y u b r count) = replacePlayer (trace (T.pack (show count)) (Player x y u True r (count +  1)) )
playerController _ (Player x y u b r count) = replacePlayer (Player x y u False r count)
 
controller :: Event -> World -> World
controller ev (World (Player px y u b r count) walls)
  | dieMode (World (Player px y u b r count) walls) px == False = 
    (World 
      (playerController ev (Player px y u b r count))
      (map (\wall -> case wall of
        (Wall x y size holePos) -> (Wall (x - 0.1) y size holePos) 
        (MoveWall x y size holePos ud) -> (MoveWall (x - 0.1) y size holePos ud))
          (dropWhile (\wall -> case wall of
            (Wall x y size holePos) -> if x < px then True else False
            (MoveWall x y size holePos ud) -> if x < px then True else False)
            walls)
            )
         )
 
  | otherwise = GameOver 0
controller (TimePassing dt) (GameOver n) = (GameOver (n + dt))
controller _ world = world
 
raund :: Double -> Picture -> Picture
raund x imag = rotated (x * pi / 180) imag 
 
class Drawable a where
  draw :: a -> Picture
 
instance Drawable Player where
  draw (Player x y u b r count) = raund r (translated 0 y (sCircle))
 
instance Drawable World where 
  draw (World player walls) = translated 3 0 ((moreWalls (take 5 walls)) <> (draw player))
  draw (GameOver x) = image1
 
instance Drawable Wall where 
  draw (Wall x y size holePos) = translated x (y + (size / 2) + 12) (rectangle 1 24) <> (translated x (y - (size / 2) - 12) (rectangle 1 24)) 
 
moreWalls :: [Wall] -> Picture
moreWalls [] = blank
moreWalls ((Wall x y size holePos):xs) = (draw (Wall x y size holePos)) <> (moreWalls xs)
moreWalls ((MoveWall x y size holePos ud):xs) = (draw (Wall x y size holePos)) <> (moreWalls xs)
 
genXSequence :: Double -> [Double]
genXSequence shift = shift : (map (+ shift) (genXSequence shift))
 
wallse :: [Double] -> [Double] -> [Double] -> [Wall]
wallse holeSizes yPositions ud =
    (map (\(f, x) -> f x) (zip wallsGenerators xSequence))
  where
    xSequence :: [Double]
    xSequence = genXSequence 12
    wallsGenerators :: [Double -> Wall]
    wallsGenerators = map 
      (\(ud, (holeSize, yPos)) -> \x -> MoveWall x yPos holeSize yPos (ud < 0.5))
      (zip ud (zip holeSizes yPositions))
 
world :: World
world = World (Player 0 0 0 False 0 0) [(MoveWall 30 0 12 0 False), (MoveWall 50 0 10 0 True), (MoveWall 100 0 8 0 True), (MoveWall 120 0 6 0 True)]
 
image9 = scaled 4 4 (clipped 5 10 (translated 7 (-5) image13))
 
gif :: Double -> Picture
gif t
  | (mod (round (t * 12)) 32) == 31 = randomName 31
  | (mod (round (t * 12)) 32) == 30 = randomName 30
  | (mod (round (t * 12)) 32) == 29 = randomName 29
  | (mod (round (t * 12)) 32) == 28 = randomName 28
  | (mod (round (t * 12)) 32) == 27 = randomName 27
  | (mod (round (t * 12)) 32) == 26 = randomName 26
  | (mod (round (t * 12)) 32) == 25 = randomName 25
  | (mod (round (t * 12)) 32) == 24 = randomName 24
  | (mod (round (t * 12)) 32) == 23 = randomName 23
  | (mod (round (t * 12)) 32) == 22 = randomName 22
  | (mod (round (t * 12)) 32) == 21 = randomName 21
  | (mod (round (t * 12)) 32) == 20 = randomName 20
  | (mod (round (t * 12)) 32) == 19 = randomName 19
  | (mod (round (t * 12)) 32) == 18 = randomName 18
  | (mod (round (t * 12)) 32) == 17 = randomName 17
  | (mod (round (t * 12)) 32) == 16 = randomName 16
  | (mod (round (t * 12)) 32) == 15 = randomName 15
  | (mod (round (t * 12)) 32) == 14 = randomName 14
  | (mod (round (t * 12)) 32) == 13 = randomName 13
  | (mod (round (t * 12)) 32) == 12 = randomName 12
  | (mod (round (t * 12)) 32) == 11 = randomName 11
  | (mod (round (t * 12)) 32) == 10 = randomName 10
  | (mod (round (t * 12)) 32) == 9 = randomName 9
  | (mod (round (t * 12)) 32) == 8 = randomName 8
  | (mod (round (t * 12)) 32) == 7 = randomName 7
  | (mod (round (t * 12)) 32) == 6 = randomName 6
  | (mod (round (t * 12)) 32) == 5 = randomName 5
  | (mod (round (t * 12)) 32) == 4 = randomName 4
  | (mod (round (t * 12)) 32) == 3 = randomName 3
  | (mod (round (t * 12)) 32) == 2 = randomName 2
  | (mod (round (t * 12)) 32) == 1 = randomName 1
  | (mod (round (t * 12)) 32) == 0 = randomName 0
  | otherwise = randomName 7
 
main :: IO ()
main = do
  gen <- getStdGen
  (gen1, gen2) <- return (split gen)
  (gen3, gen4) <- return (split gen2)
  activityOf 
    -- world
    (World
        (Player 0 0 0 False 0 0)
         (drop 2 (wallse (randomRs (9, 12) gen1) (randomRs (1, 10) gen3) (randomRs (0, 1) gen4))))
      genericController 
      (\ world -> case world of 
        (World {walls=wallss}) -> (scaled 1 (-1) (render world{walls=(take 5 wallss)}))  <> image9
        GameOver n -> gif n)
 
