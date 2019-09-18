-- {-# LANGUAGE GADTs #-}

-- module Ex06 where

-- -- Datatype of formulas
-- -- --------------------

-- data Formula ts where
--   Body   :: Term Bool                     -> Formula ()
--   Exists :: Show a 
--          => [a] -> (Term a -> Formula as) -> Formula (a, as)

-- data Term t where
--   Name    :: String -> Term t    -- to facilitate pretty printing only. 
--                                  -- don't use this in actual formulae.

--   Con     :: t -> Term t -- Constant values

--   -- Logical operators
--   And     :: Term Bool -> Term Bool -> Term Bool
--   Or      :: Term Bool -> Term Bool -> Term Bool

--   -- Comparison operators
--   Smaller :: Term Int  -> Term Int  -> Term Bool

--   -- Arithmetic operators
--   Plus    :: Term Int  -> Term Int  -> Term Int


-- -- Pretty printing formulas
-- -- ------------------------

-- instance Show t => Show (Term t) where
--   show (Con v)       = show v
--   show (And p q)     = "(" ++ show p ++ " && " ++ show q ++ ")"
--   show (Or p q)      = "(" ++ show p ++ " || " ++ show q ++ ")"
--   show (Smaller n m) = "(" ++ show n ++ " < "  ++ show m ++ ")"
--   show (Plus n m)    = "(" ++ show n ++ " + "  ++ show m ++ ")"
--   show (Name name)   = name

-- instance Show (Formula ts) where
--   show = show' ['x' : show i | i <- [0..]]
--     where
--       show' :: [String] -> Formula ts' -> String
--       show' ns     (Body body)   = show body
--       show' (n:ns) (Exists vs p) = "exists " ++ n ++ "::" ++ show vs ++ ". " ++ show' ns (p (Name n))


-- -- Example formulas
-- -- ----------------

-- ex1 :: Formula ()
-- ex1 = Body (Con False)

-- ex2 :: Formula (Int, ())
-- ex2 = Exists [1..10] $ \n ->
--         Body $ n `Smaller` (n `Plus` Con 1)

-- ex3 :: Formula (Bool, (Int, ()))
-- ex3 = Exists [False, True] $ \p -> 
--       Exists [0..2] $ \n -> 
--         Body $ p `Or` (Con 0 `Smaller` n)

-- -- Evaluating terms
-- -- ----------------
-- eval :: Term t -> t
-- eval (Con v) = v
-- eval (And b1 b2) = (eval b1) && (eval b2)
-- eval (Or b1 b2) = (eval b1) || (eval b2)
-- eval (Smaller i1 i2) = (eval i1) < (eval i2)
-- eval (Plus i1 i2) = (eval i1) + (eval i2)

-- -- the Name constructor is not relevant for evaluation
-- -- just throw an error if it is encountered:
-- eval (Name _) = error "eval: Name"   


-- -- Checking formulas
-- -- -----------------

-- satisfiable :: Formula ts -> Bool
-- satisfiable (Body tb) = eval tb 
-- satisfiable (Exists (a:as) f) = satisfiable (f (Con a)) && satisfiable (Exists as f)  

-- -- Enumerating solutions of formulae
-- -- ---------------------------------

-- solutions :: Formula ts -> [ts]
-- solutions (Body tb) = pure ()
-- solutions (Exists as f) = do
--                   h <- filter (\x -> satisfiable(f (Con x))) as
--                   t <- (\y -> solutions (f (Con y))) h
--                   (,) <$> pure h <*> pure t


-- foldleft :: (b -> a -> b) -> b -> [a] -> b 
-- foldleft f e [] = e
-- foldleft f e (x:xs) = foldleft f (f e x) xs

-- foldright :: (a-> b-> b) -> b -> [a] -> b
-- foldright f e [] = e
-- foldright f e (x:xs) =  x `f` (foldright f e xs)

-- reverse1 :: [a] -> [a]
-- reverse1 = foldr (\x ys -> ys ++ [x]) []

{-# LANGUAGE DataKinds, KindSignatures #-}

data Stream = UG | PG
data StudentID (x :: Stream) = SID Int

-- data Either a b = Left a | Right b

postgrad :: [Int]
postgrad = [3253158]

makeStudentID :: Int -> Either (StudentID UG) (StudentID PG)
makeStudentID i | i `elem` postgrad = Right (SID i) 
                | otherwise         = Left  (SID i)

enrollInCOMP3141 :: StudentID UG -> IO ()
enrollInCOMP3141 (SID x) 
  = putStrLn (show x ++ " enrolled in COMP3141!")