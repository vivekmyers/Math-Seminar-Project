import Data.Fixed
import Data.Complex
import System.IO
import Graphics.Image hiding (map)

main = do putStr "Coefficients: "
          hFlush stdout
          func <- poly . map ((:+ 0) . read) . words <$> getLine
          putStr "Window: "
          hFlush stdout
          size <- readLn
          let res = 1200 :: Double
          let result = makeImageR VU (floor res,  floor res) (solve func res (res / size)) :: Image VU RGB Double
          writeImage "winding.png" result

poly :: [Complex Double] -> Complex Double -> Complex Double
poly [] _ = 0
poly [r] _ = r
poly (r:rs:rss) x = poly ((r * x + rs):rss) x

solve :: (Complex Double -> Complex Double) -> Double -> Double -> (Int, Int) -> Pixel RGB Double
solve f s z (j, i) = toColor x' y'
  where [x, y] = [(fromIntegral k - s / 2.0) / z | k <- [i, j]]
        (x' :+ y') = f (x :+ y)

toColor :: Double -> Double -> Pixel RGB Double
toColor x y = PixelRGB r g b
  where h = (pi + atan2 y x) * 3 / pi
        dist a b = sqrt $ a ** 2 + b ** 2
        light d = 1 - (1 / (log (d + 1) + 1))
        l = light $ dist x y
        c = 1 - abs (2 * l - 1)
        v = c * (1 - abs (mod' h 2 - 1))
        res | h <= 1 = [c, v, 0]
            | h <= 2 = [v, c, 0]
            | h <= 3 = [0, c, v]
            | h <= 4 = [0, v, c]
            | h <= 5 = [v, 0, c]
            | h <= 6 = [c, 0, v]
            | otherwise = [0, 0, 0]
        m = l - c / 2
        [r, g, b] = (+m) <$> res
