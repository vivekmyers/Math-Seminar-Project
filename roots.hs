import Data.Complex
import System.IO
import Data.List
import Text.Printf
import Control.Exception
import Control.Arrow

main = handle invalid $ do
         putStr  "Coefficients: "
         hFlush stdout
         (func, dfunc) <- (poly &&& dif) . map parse . words <$> getLine
         mapM_ (putStrLn . display) (solve func dfunc (2 ^ 80))
  where invalid = const $ putStrLn "Invalid Input" :: SomeException -> IO ()

parse :: String -> Complex Double
parse s = case groupBy split s of
            ["-", "i"] -> 0 :+ (-1)
            ["+", "i"] -> 0 :+ 1
            [a, "+", "i"] -> read' a :+ 1
            [a, "-", "i"] -> read' a :+ (-1)
            ["i"] -> 0 :+ 1
            [a, b, "i"] -> read' a :+ read' b
            [b, "i"] -> 0 :+ read' b
            [a] -> read' a :+ 0
            _ -> error "Invalid Input"
  where split a b = let r = '.':['0'..'9']
                    in elem a ('+':'-':r) && elem b r
        read' ('+':s) = read' s
        read' s = read s

display :: Complex Double -> String
display (a :+ b) = case b of
                    0 -> printf "%f" a
                    _ -> case a of
                      0 -> printf "%f" b ++ "i"
                      _ -> printf "%f" a ++ (if b < 0 then "-" else "+") ++ printf "%f" (abs b) ++ "i"

type CFunc = Complex Double -> Complex Double

poly :: [Complex Double] -> Complex Double -> Complex Double
poly [] _ = 0
poly [r] _ = r
poly (r:rs:rss) x = poly ((r * x + rs):rss) x

dif r = poly $ zipWith (*) (init r) $ fromIntegral <$> iterate pred (length r - 1)

solve :: CFunc -> CFunc -> Double -> [Complex Double]
solve f df n = nub $ fmap (process 1e5) <$> solve' f ((-n) :+ n) n 
  where solve' f pt n | box f pt (2 * n) == 0 = []
                      | n < 1e-4  = [newton pt f df 1024]
                      | otherwise = let points = [pt, pt + (n :+ 0), pt + (n :+ (-n)), pt + (0 :+ (-n))]
                                    in  concat $ [solve'] <*> [f] <*> points <*> [n / 2]

newton :: Complex Double -> CFunc -> CFunc -> Integer -> Complex Double
newton x eq der it | it == 0 = x
                   | (a :+ 0) <- abs dx, a < 1e-15 = x
                   | otherwise = newton (x - dx) eq der $ pred it
  where dx = eq x / der x

process :: Double -> Double -> Double
process t n = fromIntegral (round (n * t)) / t

box :: CFunc -> Complex Double -> Double -> Double
box f pt n = process 1e5 . sum . map (uncurry $ winding f) $ zip <*> tail $ points
  where points = [pt, pt + (n :+ 0), pt + (n :+ (-n)), pt + (0 :+ (-n)), pt]

winding :: CFunc -> Complex Double -> Complex Double -> Double
winding f p1 p2 = sum angles / (2 * pi)
  where iter = 128
        points = take (iter + 1) . iterate (subtract $ (p2 - p1) / fromIntegral iter) $ p2
        segs = zip <*> tail $ points
        dtheta (a, b) = let g = phase . f
                            t = g b - g a
                        in if abs t > pi then t - 2 * pi * signum t else t
        angles = dtheta <$> segs
