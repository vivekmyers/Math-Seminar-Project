import Data.Complex
import Data.List
import Data.Ord
import Control.Monad
import Control.Parallel.Strategies
import Control.DeepSeq

main = do
        c <- map read . words <$> getLine
        n <- read <$> getLine
        z <- read <$> getLine
        i <- read <$> getLine
        let g = sortBy (comparing (negate . snd . fst)) $ grid c n z i
        forM_ (zip <*> tail $ g) $ \(((_, b), d), ((_, e), _)) ->
          if b == e
            then (putStr . show) d >> putStr " "
            else print d
        print . snd . last $ g

dif r = foldr1 ((<*>) . fmap (+)) $ r >>=
            \a -> [foldr1 ((<*>) . fmap (*)) (flip (-) <$> delete a r)]

poly r = foldr1 ((<*>) . fmap (*)) (flip (-) <$> r)

solve x r eq der it = let nx = solve' x eq der it
                          dr = (realPart . abs . subtract nx) <$> r
                      in case elemIndex (minimum dr) dr of Just ret -> ret; _ -> undefined
  where solve' x eq der 0 = x
        solve' x eq der it = let dx = if der x == 0 then x else eq x / der x
                             in solve' (x - dx) eq der $ pred it

grid :: [Complex Double] -> Double -> Double -> Integer -> [((Double, Double), Int)]
grid r n z i = let rn = (/z) <$> [(-n)..n]
                   grid = (,) <$> rn <*> rn
               in parMap rdeepseq (\z -> (,) z $ solve (uncurry (:+) z) r (poly r) (dif r) i) grid
