module Stack where

data Stack a = Stack a (Stack a) | Empty

newStack = Empty

pop :: Stack a -> Maybe (a, Stack a)
pop Empty = Nothing
pop (Stack a s) = Just (a, s)

throw :: Stack a -> Maybe (Stack a)
throw Empty = Nothing
throw (Stack a s) = Just s

push :: Stack a -> a -> Stack a
push s a = Stack a s