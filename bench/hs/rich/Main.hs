import Criterion.Main
import           EERTREE.Applications
import EERTREE.Applications (getRich, getRichParallel)

benchmark :: Int -> Benchmark
benchmark n = bench (show n) $ nf getRich n

benchmarkParallel :: Int -> Benchmark
benchmarkParallel n = bench (show n) $ nf getRichParallel n

main :: IO ()
main = defaultMain
    [bgroup "Rich"
        [benchmark  (n * 2) | n <- [1..11]],
    bgroup "Rich Parallel"
        [benchmarkParallel  (n * 2) | n <- [1..11]]
    ]
