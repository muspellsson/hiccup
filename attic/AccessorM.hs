import qualified Data.Map as M

data AccessorM t a = AccessorM { 
        getVal :: t -> Maybe a,
        setVal :: t -> a -> Maybe t
    }


data Foo = Foo { p1score_ :: Bar, p2score_ :: Int } 

data Bar = Bar { bscore_ :: Int, bcheat_ :: Bool } 


macc k = AccessorM (M.lookup k) (\x v -> Just (M.insert k v x))

testm = M.fromList [(1,"one"), (2, "two")]

accessor a b = AccessorM (\x -> Just (a x)) (\x v -> Just (b x v))

p1score = accessor p1score_ (\x v -> x { p1score_ = v }) 
bscore = accessor bscore_ (\x v -> x { bscore_ = v })
bcheat = accessor bcheat_ (\x v -> x { bcheat_ = v })


lister i = accessor (\x -> x !! i) (\x v -> (take i x) ++ (v:(drop (i+1) x)))


(.:) :: AccessorM a b -> AccessorM b c -> AccessorM a c
(.:) f g = AccessorM (\x -> getVal f x >>= getVal g) (\x v -> getVal f x >>= \fv -> setVal g fv v >>=  setVal f x)
{-# INLINE (.:) #-}


(.@) d a = (getVal a) d

(.=) (d,a) v = (setVal a) d v 
{-# INLINE (.=) #-}

(.->) = (,)


simple = Foo (Bar 3 False) 5

simple2 = simple .-> (p1score .: bcheat) .= True

main = do
  let res = show ([testm] .-> (lister 0 .: macc 9) .= "nine")
  putStrLn res
