import Tuura.Stream

test :: Stream (Int, String)
test = Join 1 (Source 2 [Event k | k <- [1..10]])
     $ Map 3 show
     $ Source 4 [Event k | k <- [101..110 :: Int]]

testGet :: Stream String
testGet = case getStream (3 :: Id String) test of
    Nothing -> error "Stream 3 not found"
    Just s  -> s

testSet :: Stream (Int, String)
testSet = setStream (Id 2 :: Id Int) (Source 5 [Event (-k) | k <- [1..10]]) test

main :: IO ()
main = do
    putStrLn $ "test = " ++ show (eval test)
    putStrLn $ "testGet = " ++ show (eval testGet)
    putStrLn $ "testSet = " ++ show (eval testSet)

