import System.IO
import Graphics.Image hiding (map, zipWith, or)
import Data.List
import Data.Function

main = do
          c <- prompt "Polynomial: " :: IO String
          let func = map read $ words c
          cx <- read <$> prompt "Center X: "
          cy <- read <$> prompt "Center Y: "
          wx <- read <$> prompt "Window X: "
          wy <- read <$> prompt "Window Y: "
          sx <- read <$> prompt "Start Range: "
          ex <- read <$> prompt "End Range: "
          let range = iterate (+(wx / 64)) sx & takeWhile (<= ex)
          let res = 1200 :: Double
          let result = makeImageR VU (floor res,  floor res) (solve range res func (cx, cy, wx, wy)) :: Image VU RGB Double
          writeImage "graph.png" result

prompt x = do putStr x
              hFlush stdout
              getLine

dif r = poly $ zipWith (*) (init r) $ fromIntegral <$> iterate pred (length r - 1)

poly :: [Double] -> Double -> Double
poly [] _ = 0
poly [r] _ = r
poly (r:rs:rss) x = poly ((r * x + rs):rss) x

shade x = PixelRGB (x * 0.5) (x * 0.5) x

tangent :: [D] -> D -> (D, D) -> Bool
tangent f x (t, y) = abs ((dif f x * (t - x) + poly f x) - y) < (dif f x + 1) * 0.01

type D = Double
solve :: [D] -> D -> [D] -> (D, D, D, D) -> (Int, Int) -> Pixel RGB Double
solve samples res f (cx, cy, wx, wy) (j, i) = let (x, y) = (cx + (fromIntegral i / res - 0.5) * wx,
                                                    cy + (fromIntegral (-j) / res + 0.5) * wy) 
                                      in if not $ abs (poly f x - y) < 0.01 * (abs (dif f x) + 1) 
                                         then if or $ tangent f <$> samples <*> pure (x, y)
                                              then shade 0.6
                                              else shade 1.0
                                         else shade 0.2 
