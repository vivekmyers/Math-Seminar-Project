import Data.Complex
import Data.List
import Data.Ord
import Control.Monad
import Control.Parallel.Strategies
import Control.DeepSeq

main = do
        c <- map read . words <$> getLine
        n <- readLn
        z <- readLn
        i <- readLn
        t <- readLn
        let g = sortBy (comparing (negate . snd . fst)) $ grid t c n z i
        forM_ (zip <*> tail $ g) $ \(((_, b), d), ((_, e), _)) ->
          if b == e
            then (putStr . show) d >> putStr " "
            else print d
        print . snd . last $ g

dif r = foldr1 ((<*>) . fmap (+)) $ r >>=
            \a -> [foldr1 ((<*>) . fmap (*)) (flip (-) <$> delete a r)]

poly r = foldr1 ((<*>) . fmap (*)) (flip (-) <$> r)

solve t x r eq der it = let nx = solve' x eq der it
                            dr = (realPart . abs . subtract nx) <$> r
                        in case elemIndex (minimum dr) dr of
                          Just ret -> if (dr !! ret) < t
                            then ret
                            else -1
                          _ -> undefined
  where solve' x eq der 0 = x
        solve' x eq der it = let dx = eq x / der x
                             in solve' (x - dx) eq der $ pred it

grid :: Double -> [Complex Double] -> Double -> Double -> Integer -> [((Double, Double), Int)]
grid t r n z i = let rn = (/z) <$> [(-n)..n]
                     grid = (,) <$> rn <*> rn
                 in parMap rdeepseq (\z -> (,) z $ solve t (uncurry (:+) z) r (poly r) (dif r) i) grid
