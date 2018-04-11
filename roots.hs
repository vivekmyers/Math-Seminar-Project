import Data.Complex
import System.IO
import Data.List
import Text.Printf

main = do putStr  "Coefficients: "
          hFlush stdout
          func <- poly . map ((:+ 0) . read) . words <$> getLine
          mapM_ (putStrLn . display) (solve func (2 ^ 60))

display :: Complex Double -> String
display (a :+ b) = case b of
                    0 -> printf "%f" a
                    _ -> case a of
                      0 -> printf "%f" b ++ "i"
                      _ -> printf "%f" a ++ (if b < 0 then "-" else "+") ++ printf "%f" (abs b) ++ "i"

poly :: [Complex Double] -> Complex Double -> Complex Double
poly [] _ = 0
poly [r] _ = r
poly (r:rs:rss) x = poly ((r * x + rs):rss) x

solve :: (Complex Double -> Complex Double) -> Double -> [Complex Double]
solve f n = nub $ fmap (process 1e6) <$> solve' f ((-n) :+ n) n
  where solve' f pt n | box f pt (2 * n) == 0 = []
                      | n < 2e-7  = [pt]
                      | otherwise = let (p1, p2, p3, p4) = (pt, pt + (n :+ 0), pt + (n :+ (-n)), pt + (0 :+ (-n)))
                                    in concat [solve' f p1 (n / 2), solve' f p2 (n / 2), solve' f p3 (n / 2), solve' f p4 (n / 2)]

process :: Double -> Double -> Double
process t n = fromIntegral (round (n * t)) / t

box :: (Complex Double -> Complex Double) -> Complex Double -> Double -> Double
box f pt n = process 1e2 . sum . map (uncurry $ winding f) $ zip <*> tail $ points
  where points = [pt, pt + (n :+ 0), pt + (n :+ (-n)), pt + (0 :+ (-n)), pt]

winding :: (Complex Double -> Complex Double) -> Complex Double -> Complex Double -> Double
winding f p1 p2 = sum angles / (2 * pi)
  where iter = 128
        points = take (iter + 1) . iterate (subtract $ (p2 - p1) / fromIntegral iter) $ p2
        segs = zip <*> tail $ points
        dtheta (a, b) = let g = phase . f
                            t = g b - g a
                        in if abs t > pi then t - 2 * pi * signum t else t
        angles = dtheta <$> segs
