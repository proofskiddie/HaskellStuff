{-# LANGUAGE OverloadedStrings #-}
import CodeWorld

botGreen, topRed, frame, trafficLight :: Picture
botGreen = colored green (translated 0 (-1.5) (solidCircle 1))
topRed = colored red (translated 0 (1.5) (solidCircle 1))
frame = rectangle 2.5 5.5
trafficLight = botGreen & topRed & frame

ourPicture :: Picture
ourPicture = trafficLight

main :: IO ()
main = drawingOf ourPicture
