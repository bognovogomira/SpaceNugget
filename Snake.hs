module Snake where

import Graphics.Gloss
import qualified Graphics.Gloss.Data.Point.Arithmetic as Arith
import Graphics.Gloss.Data.Vector

import System.Random

type Length      = Int
type Girth       = Float
type Speed       = Float
type Opacity     = Float
type Depth       = Float

remap :: Float -> (Float, Float) -> (Float, Float) -> Float
remap p (a,b) (c,d) = c + (d-c)*(p-a)/(b-a)

--

type Snake = [(Point, Girth, Opacity)]

showSnake :: Color -> Girth -> Snake -> Picture
showSnake col girth snake = Pictures $
                    map (\(x,g,o) -> uncurry translate x (color (withAlpha o col) (circleSolid (g*girth))))
                    snake

updateSnake :: Length -> Snake -> Snake
updateSnake n = take n . map (\(p,g,c) -> (p, g * 0.95, 0.95*c))

growSnake :: Point -> Snake -> Snake
growSnake point = (:) (point,1,1)

--

type Meteor = (Snake, Speed, Girth, Point, Depth)

showMeteor :: Meteor -> Picture
showMeteor (m, _, g, _, d) = showSnake red girth m
  where girth = remap ((d/500)**0.4) (1, 0) (0, g)

showMeteors :: ([Meteor],StdGen) -> Picture
showMeteors (ms,_) = Pictures $ map showMeteor ms
 
updateMeteor :: Meteor -> Meteor
updateMeteor (ms, v, g, (r, theta), d) = (updateSnake 50 (newhead :ms), v, g, (r, theta), d - v)
  where newx = remap ((d/500)**0.4) (1, 0) (r* cos theta, 900 * cos theta)
        newy = remap ((d/500)**0.4) (1, 0) (r* sin theta, 900 * sin theta)
        newhead = ((newx, newy), 1, 1)

inBounds :: Meteor -> Bool
inBounds (m,_,_,_,_) = let ((x,y),g,o) = head m
                       in null m || ((abs x < 900) && (abs y < 500))

createMeteor :: StdGen -> (Meteor, StdGen)
createMeteor g = let (theta, g1) = randomR (0, 2* pi) g
                     (r, g2) = randomR (20, 200) g1
                     (v, g3) = randomR (0.5,2) g2
                     (girth, g4) = randomR (50, 200) g3
                     in (([], v, girth, (r,theta), 500), g4)

addMeteors :: Int -> ([Meteor], StdGen) -> ([Meteor], StdGen)
addMeteors 0 (ms, g) = (ms, g)
addMeteors 1 (ms, g) = let (m, g1) = createMeteor g in (m:ms, g1)
addMeteors n x       = addMeteors (n-1) (addMeteors 1 x)

  
updateAllMeteors :: ([Meteor], StdGen) -> ([Meteor], StdGen)
updateAllMeteors (ms, g) = let updatedms = takeWhile inBounds (map updateMeteor ms)
                               (p, g1) = randomR (0,1) g :: (Float, StdGen)
                               (n, g2) = randomR (1,4) g1 :: (Int, StdGen)
                           in if null updatedms then addMeteors 1 (updatedms, g)
                              else if p > 0.001 then (updatedms, g1)
                              else addMeteors n (updatedms, g2)

hit :: Snake -> Girth -> Meteor -> Bool
hit _ _ ([], _, _, _, _) = False
hit [] _ _ = False
hit (x:xs) gs (y:ys, _, gm, _, d) = let psnake = (\(a,b,c)-> a) x
                                        pmet   = (\(a,b,c)-> a) y
                                        curgirth = remap ((d/500)**0.4) (1, 0) (0, gm)
                                     in magV ((Arith.-) psnake pmet) < (gs + curgirth) * 0.95

type Star = (Point, Depth, Speed, StdGen)

showStar :: Star -> Picture
showStar ((r,theta), d, _, _) = color white $ translate x y $ circleSolid 1
  where x = remap ((d/500)**0.3) (1, 0) (r * cos theta, 900 * cos theta)
        y = remap ((d/500)**0.3) (1, 0) (r * sin theta, 900 * sin theta)

onScreen :: Star -> Bool
onScreen ((r,theta),d, _, _) = (abs x < 750) && (abs y < 450)
  where x = remap ((d/500)**0.3) (1, 0) (r * cos theta, 900 * cos theta)
        y = remap ((d/500)**0.3) (1, 0) (r * sin theta, 900 * sin theta)

updateStar :: Star -> Star
updateStar (p, d, v, g) = if onScreen (p,d,v,g)
                          then (p, d-v, v, g)
                          else let (r, g1) = randomR (20, 700) g
                                   (theta, g2) = randomR (0,2*pi) g1
                                   (v', g3) = randomR (0.5, 1.5) g2
                               in ((r, theta), 500, v', g3)

starList :: StdGen -> Int -> [Star]
starList _ 0 = []
starList g n = let (r,g1) = randomR (20, 900) g
                   (theta, g2) = randomR (0, 2*pi) g1
                   (v, g3) = randomR (0.5,1.5) g2
                   (g4, g5) = split g3
               in ((r,theta), 500, v, g4) : starList g5 (n-1)

updateStarList = map updateStar
showStarList = Pictures . map showStar
