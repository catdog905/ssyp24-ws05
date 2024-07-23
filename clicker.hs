{-# LANGUAGE OverloadedStrings #-}
import CodeWorld
import CodeWorld.Image
import qualified Data.Text as T
 
 
 
 
data World = World (String, String, Int, Int) Int 
 
clicer :: Picture
clicer = colored (darker 0.3 yellow) (solidRectangle 1 1) 
 
 
controller :: Event -> World -> World
controller (PointerPress _ ) (World (name, url, x, y) count) 
  |  count == 20000 = World ("10", url10, 20, 20) (count + 1)
  |  name == "10" && count >= 20000 = World ("8", url8, 20, 20) (count + 1)
  |  name == "8" && count >= 20000 = World ("9", url9, 20, 20) (count + 1)
  |  name == "9" && count >= 20000 = World ("10", url10, 20, 20) (count + 1)
  |  count `mod` 5 == 0 && count >= 10000 = World ("3", url3, 20, 20) (count + 1)
  |  count `mod` 5 == 1 && count >= 10000 = World ("4", url4, 20, 20) (count + 1)
  |  count `mod` 5 == 2 && count >= 10000 = World ("5", url5, 20, 20) (count + 1)
  |  count `mod` 5 == 3 && count >= 10000 = World ("6", url6, 20, 20) (count + 1)
  |  count `mod` 5 == 4 && count >= 10000 = World ("7", url7, 20, 20) (count + 1)
 
 
 
controller (PointerPress _) (World ("first", url, x, y) count) =  World ("second", url2, 20, 25) (count + 1)
controller (PointerPress _) (World ("second", url, x, y) count) =  World ("first", url1, 20, 25) (count + 1)
controller (PointerPress _) (World (name, url, x, y) count) =  World (name, url, 20, 25) (count + 1)
--controller (KeyPress "W") (World (name, url, x, y) phrase count) =  World (name, url1,20, 25) "1"  (count + 1)
 
controller _ world = world
 
 
-- main :: IO ()
-- main = activityOf
 
initialState :: World
initialState = World ( "first", url1, 20, 25 ) 0
 
drawWorld ::  World  -> Picture
drawWorld (World (name, url, x, y) count) = (translated (- 0.5) 8 ((lettering (T.pack  (show count))) <> (colored yellow (solidRectangle 5 2)))) <> (image (T.pack name) (T.pack url) (fromIntegral x) (fromIntegral y))
 
url10 = "https://64.media.tumblr.com/a6f642dc72cfabd368ca4d82ce3e79e4/50c152f69306d3e4-f2/s2048x3072/b9102662b9188d031385bc1c9457d98c90b66869.jpg"
url9 = "https://kartinki.pics/uploads/posts/2022-09/thumbs/1662345389_40-kartinkin-net-p-endzhel-dast-art-oboi-47.jpg"
url8 = "https://sun9-1.userapi.com/impg/bKBImV5V7fVMWSM6VhDNK3eAHXQBO5b8mOjlXg/pkmP5SMq02I.jpg?quality=96&as=32x23,48x34,72x51,108x77,160x114,240x172,360x257,480x343,540x386,640x457,720x515,1080x772&sign=d7161c87154c6eb153ddd20aa2ddfe97&from=bu&u=-RZdWC17bq_J5FnAxSgwzE5X7BfdQdljVRA_Jy_yKUc&cs=604x432"
url7 = "https://i.ytimg.com/vi/g0MXK-YUpyM/maxresdefault.jpg?sqp=-oaymwEmCIAKENAF8quKqQMa8AEB-AH-CYAC0AWKAgwIABABGH8gOSgtMA8=&rs=AOn4CLAJPIZP6g3gU6BRH-xYsAEKalvmig"
url6 = "https://64.media.tumblr.com/b9c630b29ce3b75b08415fead62bfc28/802a2ef0c605ec5e-68/s1280x1920/456e39ebdd0d5652052681fd0e6b738eb6b9b09c.jpg"
url5 = "https://media.tenor.com/LDs7lkmik_MAAAAe/playing-piano-alastor.png"
url4 = "https://encrypted-tbn0.gstatic.com/images?q=tbn:ANd9GcQQ63PAcutKEc_CmPFPoqU5gAC_943P_ocfgg&s"
url3 = "https://i.ytimg.com/vi/IbjKxxQFM3A/maxresdefault.jpg?sqp=-oaymwEmCIAKENAF8quKqQMa8AEB-AH-CYAC0AWKAgwIABABGH8gEyg-MA8=&rs=AOn4CLB5DD3kGYVD4j_5WgRqwmTMjS7bPQ"
url1 = "https://fbi.cults3d.com/uploaders/32787555/illustration-file/a9811298-bb45-40ce-9914-e8d023033492/GFwMVweWIAAVd14.jpg" 
url2 = "https://static.wikia.nocookie.net/hazbinhotel/images/7/79/Angel_Sir_Pentious.png"
main = activityOf initialState controller drawWorld
