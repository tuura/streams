import Tuura.Stream

-- Various event sources
chars :: [Event Char]
chars = map Event "abcde"

posInts :: [Event Int]
posInts = map Event [1..5]

negInts :: [Event Int]
negInts = map (fmap negate) posInts

bools :: [Event Bool]
bools = map Event $ cycle [True, False]

-- Test streams
test :: Stream (Char, String)
test = Join 1 (Source 2 chars) (Map 3 show $ Source 4 posInts)

testGet :: Stream String
testGet = case getStream 3 test of
    Nothing -> error "Stream 3 not found"
    Just s  -> s

testSet :: Stream (Char, String)
testSet = setStream 2 (Source 5 negInts) test

testFun :: Stream (Bool -> String)
testFun = Map 6 choose test
  where
    choose (c, s) = \x -> if x then c:[] else s

testApp :: Stream String
testApp = Map 7 app $ Join 8 testFun (Source 9 bools)
  where
    app (f, x) = f x

main :: IO ()
main = do
    putStrLn $ "test    = " ++ show (eval test   )
    putStrLn $ "testGet = " ++ show (eval testGet)
    putStrLn $ "testSet = " ++ show (eval testSet)
    putStrLn $ "testApp = " ++ show (eval testApp)
