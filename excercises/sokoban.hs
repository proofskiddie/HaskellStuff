import CodeWorld

wall    :: Picture
wall    =  colored yellow (rectangle 1.0 1.0)

ground  :: Picture
ground  =  colored brown (rectangle 1.0 1.0)

storage :: Picture
storage =  colored blue (solidCircle (1.0/8))

box     :: Picture
box     =  colored orange (rectangle 1.0 1.0)

drawTile :: Integer -> Picture
drawTile n
  | n == 1 = wall
  | n == 2 = ground
  | n == 3 = storage
  | n == 4 = box
  | otherwise = blank

maze    :: Double -> Double -> Integer 
maze x y
  | abs x > 4.0  || abs y > 4.0  = 0 
  | abs x == 4.0 || abs y == 4.0 = 1
  | x == 2.0  && y <= 0.0        = 1
  | x == 3.0  && y <= 0.0        = 3
  | x >= -2.0 && y <= 0.0        = 4
  | otherwise                    = 2

recurseRow :: Double -> Double -> Picture
recurseRow _ (-10.0) = blank
recurseRow x y = (translated x y (drawTile (maze x y))) & recurseRow x (y - 1) 

recurseGrid :: Double -> Double -> Picture
recurseGrid (-10.0) _ = blank
recurseGrid x y = recurseRow x y & recurseGrid (x - 1) y   


pictureOfMaze :: Picture
pictureOfMaze = recurseGrid 10.0 10.0 

main :: IO()
main = drawingOf pictureOfMaze

