module Ex04 where

import Text.Read (readMaybe)

data Token = Number Int | Operator (Int -> Int -> Int)

parseToken :: String -> Maybe Token
parseToken "+" = Just (Operator (+))
parseToken "-" = Just (Operator (-))
parseToken "/" = Just (Operator div)
parseToken "*" = Just (Operator (*))
parseToken str = fmap Number (readMaybe str)

tokenise :: String -> Maybe [Token]
tokenise x = sequence (fmap parseToken (words x)) 


newtype Calc a = C ([Int] -> Maybe ([Int], a))

pop :: Calc Int
pop = C f 
  where 
    f (x:xs) = Just (xs, x)
    f _ = Nothing

push :: Int -> Calc ()
push i = C f
  where
    f xs = Just ((i:xs), ())
    

-- sa :: [Int] -> Maybe ([Int], a)
instance Functor Calc where
  fmap f (C sa) = C $ \s ->
      case sa s of 
        Nothing      -> Nothing
        Just (s', a) -> Just (s', f a)

instance Applicative Calc where
  pure x = C (\s -> Just (s,x))
  C sf <*> C sx = C $ \s -> 
      case sf s of 
          Nothing     -> Nothing
          Just (s',f) -> case sx s' of
              Nothing      -> Nothing
              Just (s'',x) -> Just (s'', f x)
-- Calc a -> (a -> Calc b) -> Calc b
instance Monad Calc where
  return = pure
  C sa >>= f = C $ \s -> 
      case sa s of 
          Nothing     -> Nothing
          Just (s',a) -> unwrapCalc (f a) s'
    where unwrapCalc (C a) = a

evaluate :: [Token] -> Calc Int
evaluate [] = pop
evaluate (t:ts) = case t of
  Number n -> do push n >> (evaluate ts)
  Operator o -> do y <- pop
                   x <- pop
                   push ((o) x y) >> (evaluate ts)

calculate :: String -> Maybe Int
calculate s = case (tokenise s) of
  Nothing -> Nothing
  Just ts -> fmap snd ((unwrapCalc (evaluate ts)) [])
          where unwrapCalc (C a) = a

data Car = Car { getCar :: [Char],
                getNum ::Int } deriving (Show)

justH :: Maybe Char
-- justH = Just "hello" >>= (\(x:xs) -> Just x)
justH = do
  (x:xs) <- Just "hello"
  Just x

marySue :: Maybe Bool
marySue = Just 9 >>= (\x -> Just (x > 8))

mapM' ::Monad m =>(a -> m b) -> [a] -> m [b]
mapM' f [] = pure []
-- x' <- f x notation just peel the value out of the monad
-- since (\x -> f x) :: a -> m b
-- input must be a monad and the output also should be a monad
-- as (>>=) :: m a -> (a -> m b) -> m b 
mapM' f (x:xs) = f x >>= (\x' -> mapM' f xs >>= \xs' -> pure (x':xs'))

data NonEmptyList a = One a | Cons a (NonEmptyList a) deriving (Show)
instance Functor NonEmptyList where
  fmap f (One x) = One (f x)
  fmap f (Cons x xs) = Cons (f (f x)) (fmap f xs)
runCalc :: Calc a -> [Int] -> Maybe ([Int],a)
runCalc (C x) = x


pair :: (Applicative f) => f a -> f b -> f (a, b)
pair fa fb = pure (,) <*> pure fa <*> pure fb