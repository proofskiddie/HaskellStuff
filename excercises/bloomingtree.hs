import CodeWorld


finalSize :: Double 
finalSize = (1/4)

tree ::  Picture -> Integer -> Picture
tree pic 0 = pic 
tree pic n = path [(0,0),(0,1)] & translated 0 1 (
  rotated (pi/10) (tree pic (n-1) ) & rotated (- pi/10) (tree pic (n-1) )) 

bloom :: Double -> Picture
bloom n
  | n < 10    = colored green (solidCircle (finalSize * (n/10)))
  | otherwise = colored green (solidCircle (finalSize)) 

myanimation :: Double -> Picture
myanimation t = tree (bloom t) 8

main :: IO ()
main = animationOf myanimation 
