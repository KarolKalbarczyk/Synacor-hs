{-# LANGUAGE GADTs, StandaloneDeriving, FlexibleInstances #-}

module Types where

maxNumb= 32767
minReg = 32768
maxReg = 32775
range :: Int -> Int -> Int -> Bool
range i min max = i >= min && i <= max

data Basic a where
  Number :: Int -> Basic Numb
  Register :: Int -> Basic Reg

deriving instance Show (Basic a)
deriving instance Eq (Basic a)
deriving instance Ord (Basic a)
instance Num (Basic Numb) where
 (Number n1) + (Number n2) = Number $ (n1 + n2) `mod` minReg
 (Number n1) * (Number n2) = Number $ (n1 * n2) `mod` minReg
 (Number n1) - (Number n2) = Number $ abs $ (n1 - n2) `mod` minReg

newtype Reg = Reg Int
newtype Numb = Numb Int

getRegister :: Int -> Maybe (Basic Reg)
getRegister i = if range i minReg maxReg then Just $ Register (i - minReg) else Nothing

getNumber :: Int -> Maybe (Basic Numb)
getNumber i = if range i 0 maxNumb then Just $ Number i else Nothing

data Basica = B1 (Basic Numb) | B2 (Basic Reg) deriving Show

getBasic :: Int -> Maybe (Basica)
getBasic i = if range i 0 maxNumb then (Just $ B1 $ Number i)
              else if range i minReg maxReg then (Just $ B2 $ Register (i - minReg))
              else Nothing

toBasic :: Basic a -> Basica
toBasic n@(Number _) = B1 n
toBasic r@(Register _) = B2 r

stripNumb:: Basic Numb -> Int
stripNumb (Number x) = x

newtype RegisterState = RegisterState [Basic Numb]

getRegValue :: Basic Reg -> RegisterState -> Basic Numb
getRegValue (Register r) (RegisterState s) = s !! r

modifyRegs :: Basic Reg -> (Basic Numb-> Basic Numb) -> RegisterState -> RegisterState
modifyRegs (Register i) f (RegisterState rs) = RegisterState $ modifyList i rs (\x _ -> f x)

modifyList :: Int -> [a] -> (a -> Int -> a) -> [a]
modifyList index list f = fmap (\(num, i) ->  if index == i then f num i else num)  $ zipWith (\x i -> (x,i)) list [0..]