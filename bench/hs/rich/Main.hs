-- import           Data.Proxy      (Proxy (..))
import           EERTREE.Simple

import           Criterion.Main

benchmark :: [Int] -> Benchmark
benchmark inputValues = bgroup "a216264" [ bench (show n) $ whnf a216264 n | n <- inputValues ]

main :: IO ()
main = defaultMain [benchmark [2, 4, 6, 8, 10, 12, 14, 16, 18, 20, 22]]
