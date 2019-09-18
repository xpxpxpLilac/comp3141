import Data.Char
import Data.List
import Test.QuickCheck

rot13 :: String -> String
rot13 = map $ \x -> 
          case lookup x table of
            Just y  -> y 
            Nothing -> x
  where
    table = table' 'A' 'Z' ++ table' 'a' 'z'
    table' a z = zip [a..z] (drop 13 (cycle [a..z]))

rot13Spec1 x = length x == length (rot13 x)
rot13Spec2 x = rot13 (map toUpper x) == map toUpper (rot13 x)
rot13Spec3 f x = rot13 (map f x) == map f (rot13 x)
rot13Spec4 a b = rot13 (a ++ b) == rot13 a ++ rot13 b
rot13Spec5 x = all (not . isAlpha) (x:: String) ==> rot13 x == x
rot13Spec6 x = rot13 (map toUpper x) == map toUpper (rot13 x)
rot13Spec7 x = not (null x) ==> ord (head x) + 13 == ord (head (rot13 x)) 
rot13Spec8 x = rot13 (rot13 x) == x


merge :: (Ord a) => [a] -> [a] -> [a]
merge (x:xs) (y:ys) | x < y     = x:merge xs (y:ys)
                    | otherwise = y:merge (x:xs) ys
merge xs [] = xs
merge [] ys = ys

mergeSpec1 a b = merge (sort a) (sort b) == sort (merge a b)
mergeSpec2 a b = merge a b == sort (a ++ b)
mergeSpec3 a b = length (merge a b) == length a + length b
-- mergeSpec4 a b = merge (filter f a) (filter f b) == filter f (merge a b)
-- mergeSpec5 a b = merge (map f a) (map f b) == map f (merge a b)
mergeSpec6 a b = sort (merge a b) == sort (a ++ b)


toBinary :: Int -> String
toBinary 0 = ""
toBinary n = let (d,r) = n `divMod` 2
              in toBinary d 
                   ++ if r == 0 then "0"
                                else "1"

fromBinary :: String -> Int
fromBinary = fst . foldr eachChar (0,1)
  where
    eachChar '1' (sum, m) = (sum + m, m*2)
    eachChar _   (sum, m) = (sum    , m*2)



binSpec1 s = all (`elem` "01") s ==> toBinary (fromBinary s) == s
binSpec2 s = all (`elem` "01") s ==> read s >= fromBinary s 
binSpec3 s = all (`elem` "01") s ==> fromBinary s == fromBinary ('0':s)
binSpec4 i = i > 0 ==> length (toBinary i) >= length (show i) 
binSpec5 i = i >= 0 ==> fromBinary (toBinary i) == i
elem01 :: (String -> Bool) -> String -> Bool
elem01 f s = f s

foo :: [a] -> (a -> b) -> [b]
foo [] f = []
foo (x:xs) f = foo xs f

type Graph = [[Bool]]

m :: Graph
m = [[False, True,  True,  False],
     [True,  False, False, True ],
     [True,  False, False, True ],
     [False, True,  True,  False]]

graphSpec1 g = all (\x -> length x == length g) g