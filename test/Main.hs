import Tuura.Stream

chars :: [Event Char]
chars = [ Event c | c <- "abcdefghij" ]

posInts :: [Event Int]
posInts = [ Event k | k <- [1..10] ]

negInts :: [Event Int]
negInts = map (fmap negate) posInts

test :: Stream (Char, String)
test = Join 1 (Source 2 chars) (Map 3 show $ Source 4 posInts)

testGet :: Stream String
testGet = case getStream 3 test of
    Nothing -> error "Stream 3 not found"
    Just s  -> s

testSet :: Stream (Char, String)
testSet = setStream 2 (Source 5 negInts) test

main :: IO ()
main = do
    putStrLn $ "test    = " ++ show (eval test   )
    putStrLn $ "testGet = " ++ show (eval testGet)
    putStrLn $ "testSet = " ++ show (eval testSet)
