module Main where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Data.Vector
import qualified Graphics.Gloss.Data.Point.Arithmetic as Arith
import System.Random

import Snake

type Nugget = (Point, StdGen)

showNugget :: Nugget -> Picture
showNugget (n,g) = color yellow (uncurry translate n (circleSolid (fst (randomR (5,15) g))))

newNugget :: Nugget -> Nugget
newNugget (n,g) = ((coord1, coord2), g3)
  where (coord1, g2) = randomR (-700,700) g
        (coord2, g3) = randomR (-400,400) g2

type World = (Snake, (Color, Girth, Length), Nugget, Message, ([Meteor], StdGen), Int, [Star])

eaten :: World -> Bool
eaten ([], _, _, _, _, _, _) = False
eaten ((p,_,_):_, (_,g,_), (n,_), _, _, _, _) = magV ((Arith.-) p n) < (0.95 * g)

crash :: World -> Bool
crash (snake, (_, g, _), _, _, (ms,_), _, _) = any (hit snake g) ms

type Message = (Picture, Float)

start :: Message
start = (color white $ Pictures 
        [translate (negate 400) 0 (scale 0.6 0.3 (text "NUGGET EATER 1.0")),
        translate (negate 200) (negate 70) (scale 0.3 0.15 (text "by parth shimpi"))],
        2.0)

loss :: Int -> Message
loss score = (color white $ Pictures 
       [translate (negate 450) 0 (scale 0.3 0.25 (text $ "You ate " ++ show score ++ " nuggets before a meteor hit you."))], 2)

plot :: World -> Picture
plot (snake, (c,g,l), nugget, (message, opacity), mets, score, stars) = Pictures 
                                                     [ message,
                                                       color (withAlpha (1- opacity) black) (rectangleSolid 2000 1000),
                                                       showStarList stars,
                                                       showNugget nugget,
                                                       showMeteors mets,
                                                       showSnake c g snake,
                                                       scale 0.5 0.7 (color white (translate 900 400 (text (show score))))]

handleKeys :: Event -> World -> World
handleKeys (EventMotion here) (snake, p, n, m, ms, s, sl) = (growSnake here snake , p, n, m, ms, s, sl)
handleKeys (EventKey (Char '0') Down _ _) (sn, (c,g,l), n, m, ms, s, sl) = (sn, (aquamarine, g, l), n, m, ms, s, sl)
handleKeys (EventKey (Char '1') Down _ _) (sn, (c,g,l), n, m, ms, s, sl) = (sn, (light azure, g, l), n, m, ms, s, sl)
handleKeys (EventKey (Char '2') Down _ _) (sn, (c,g,l), n, m, ms, s, sl) = (sn, (light $ light red, g, l), n, m, ms, s, sl)
handleKeys (EventKey (Char '3') Down _ _) (sn, (c,g,l), n, m, ms, s, sl) = (sn, (light violet, g, l), n, m, ms, s, sl)
handleKeys (EventKey (Char '4') Down _ _) (sn, (c,g,l), n, m, ms, s, sl) = (sn, (orange, g, l), n, m, ms, s, sl)
handleKeys (EventKey (Char '5') Down _ _) (sn, (c,g,l), n, m, ms, s, sl) = (sn, (rose, g, l), n, m, ms, s, sl)
handleKeys (EventKey (Char '6') Down _ _) (sn, (c,g,l), n, m, ms, s, sl) = (sn, (light cyan, g, l), n, m, ms, s, sl)
handleKeys (EventKey (Char '7') Down _ _) (sn, (c,g,l), n, m, ms, s, sl) = (sn, (magenta, g, l), n, m, ms, s, sl)
handleKeys (EventKey (Char '8') Down _ _) (sn, (c,g,l), n, m, ms, s, sl) = (sn, (light green, g, l), n, m, ms, s, sl)
handleKeys (EventKey (Char '9') Down _ _) (sn, (c,g,l), n, m, ms, s, sl) = (sn, (white, g, l), n, m, ms, s, sl)
handleKeys _ x = x

updateFrame :: Float -> World -> World
updateFrame _ world = let (snake, (c,g,l), nugget, (msg, opacity), mets, score, stars) = world
                          newmets = updateAllMeteors mets
                          newSnake = updateSnake l snake
                          newStars = updateStarList stars
                      in if crash world
                         then (snake, (c,20,50), nugget, loss score, newmets, 0, newStars)
                         else if eaten world
                              then (newSnake, (c, (5+ g** (100/95))**0.95, l+20), newNugget nugget, (msg, opacity * 0.995), newmets, score + 1, newStars )
                              else (newSnake, (c,g,l), nugget, (msg, opacity * 0.994), newmets, score, newStars)

initialWorld :: World
initialWorld = ([], (aquamarine , 15, 50), ((0,0), mkStdGen 900), start, ([], mkStdGen 50), 0, starList (mkStdGen 10) 150 )

main :: IO ()
main = play FullScreen black 120 initialWorld plot handleKeys updateFrame
