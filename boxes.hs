import Data.Complex
import System.IO
import Data.List
import Text.Printf
import Control.Exception

main = handle invalid $ do
         putStr  "Coefficients: "
         hFlush stdout
         func <- poly . map parse . words <$> getLine
         bounds <- readLn
         writeFile "boxes.js" . ("window.t = "++) . map (\a -> if a == '\'' then '"' else a) . filter (/= '"') . show $ do
            r <- solve func bounds
            return $ (\(a :+ b) -> "{ 'x': " ++ show a ++ ", 'y': " ++ show b ++ " }" ) <$> r
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

poly :: [Complex Double] -> Complex Double -> Complex Double
poly [] _ = 0
poly [r] _ = r
poly (r:rs:rss) x = poly ((r * x + rs):rss) x

solve :: (Complex Double -> Complex Double) -> Double -> [[Complex Double]]
solve f n = solve' f ((-n) :+ n) n True
  where solve' f pt n b | box f pt (2 * n) == 0 && b = [[pt, pt + (2*n :+ 0), pt + (2*n :+ (-2*n)), pt + (0 :+ (-2*n)), 0]]
                        | n < 2e-8  = [[pt, pt + (2*n :+ 0), pt + (2*n :+ (-2*n)), pt + (0 :+ (-2*n)), 0]]
                        | otherwise = let points = [pt, pt + (n :+ 0), pt + (n :+ (-n)), pt + (0 :+ (-n))]
                                      in  (:)
                                        [pt, pt + (2*n :+ 0), pt + (2*n :+ (-2*n)), pt + (0 :+ (-2*n))] $
                                        concat $ [solve'] <*> [f] <*> points <*> [n / 2] <*> [True]

process :: Double -> Double -> Double
process t n = fromIntegral (round (n * t)) / t

box :: (Complex Double -> Complex Double) -> Complex Double -> Double -> Double
box f pt n = process 1e5 . sum . map (uncurry $ winding f) $ zip <*> tail $ points
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
