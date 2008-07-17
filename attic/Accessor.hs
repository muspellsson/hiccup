
data Accessor t a = Accessor { 
        getVal :: t -> a,
        setVal :: t -> a -> t
    }


data Foo = Foo { p1score_ :: Bar, p2score_ :: Int } deriving Show

data Bar = Bar { bscore_ :: Int, bcheat_ :: Bool } deriving Show


accessor a b = Accessor (\x -> a x) (\x v -> b x v)

p1score = accessor p1score_ (\x v -> x { p1score_ = v }) 
p2score = accessor p2score_ (\x v -> x { p2score_ = v }) 
bscore = accessor bscore_ (\x v -> x { bscore_ = v })
bcheat = accessor bcheat_ (\x v -> x { bcheat_ = v })


(.:) :: Accessor a b -> Accessor b c -> Accessor a c
(.:) f g = Accessor (\x -> (getVal g) (getVal f x)) (\x v -> setVal f x (setVal g (getVal f x) v))
{-# INLINE (.:) #-}


(.@) d a = (getVal a) d

(.=) (d,a) v = (setVal a) d v 
{-# INLINE (.=) #-}

(.->) = (,)


simple = Foo (Bar 3 False) 5

simple2 = simple .-> (p1score .: bcheat) .= True

main = do
  let res = show simple2
  putStrLn res
