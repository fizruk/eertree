import Criterion.Main
import           EERTREE.Applications

benchmark :: Int -> Benchmark
benchmark n = bench (show n) $ nf getRich n

main :: IO ()
main = defaultMain
    [bgroup "Rich"
        [benchmark  (n * 2) | n <- [1..11]]
    ]
