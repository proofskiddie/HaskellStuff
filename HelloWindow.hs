
import Graphics.UI.GLUT
import Graphics.Rendering.OpenGL

main = do
  getArgsAndInitialize
  createWindow "Hello Window"
  displayCallback $= displayPoints
  mainLoop

myPoints :: [(GLfloat,GLfloat,GLfloat)]
myPoints = [ (-0.25,0.25,-0.0),(0.75,0.35,0.0),(0.75,-0.15,0.0) ]

displayPoints = do
  clear [ColorBuffer]
  renderPrimitive Points
    $mapM_ (\(x,y,z) -> vertex$Vertex3 x y z) myPoints
    

