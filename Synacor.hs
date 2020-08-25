{-# LANGUAGE GADTs, StandaloneDeriving, FlexibleInstances #-}

module Synacor where

import Data.Char
import Control.Monad.Trans.Maybe
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.State
import Data.Maybe
import Control.Monad.Except
import Control.Monad.Identity
import Control.Monad.Reader
import Stack
import Types

uncurry3 f (a, b, c) = f a b c

toExcept :: Monad m => Maybe a -> ExceptT String m a
toExcept (Just x) = return x
toExcept _ = throwError "dsasdds"

reportMistake :: Int -> IO ()
reportMistake = putStrLn . ((<>) "wrong number of arguments for opcode ") . show

start = (flip runStateT) (RegisterState $ catMaybes $ fmap getNumber [0..7]) . runExceptT

(>>>) :: Monad m => (m a, m b) -> ((a, b) -> m c) -> m c
(a, b) >>> f = a >>= (\m1 -> (b >>= (\m2 -> f (m1, m2) )))

(>>:) :: Monad m => (m a, m b, m c) -> ((a, b, c) -> m d) -> m d
(a, b, c) >>: f = a >>= (\m1 -> (b >>= (\m2 -> (c >>= (\m3 -> f (m1, m2, m3))))))

infixl 8 >>>
infixl 8 >>:

repl' :: IO ()
repl' = do
        op <- liftIO getLine
        let (last, numbers) = foldr (\char (current, list) -> if char `elem` ",\n" then ("", current:list) else (char:current, list)) ([], []) op
        let  opcodes = fmap (\x -> read x :: Int) (last:numbers)
        (flip runStateT) (RegisterState $ catMaybes $ fmap getNumber [0..7]) . (flip runReaderT) opcodes . runExceptT $ process' opcodes Empty
        repl'


process' :: [Int] -> Stack (Basic Numb) -> ExceptT String (ReaderT [Int] (StateT RegisterState IO)) ()
process' opcodes stack = case opcodes of
                  [] -> return ()
                  (0:t) -> (liftIO $ putStrLn "bye") >> return ()
                  (1:r1:r2:t) -> (getReg r1, getBas r2) >>> lift2 . (uncurry executeIn2) >> process' t stack
                  (2:x:t) -> getBas x >>= lift2 . getValue >>= process' t . push stack
                  (3:x:t) -> (getReg x, toExcept $ pop stack) >>> \(r, (n, stack')) -> (lift2 $ executePop n r) >> process' t stack'
                  (4:a:b:c:t) -> (getReg a, getBas b, getBas c) >>: lift2  . uncurry3  (executeComp (==)) >> process' t stack
                  (5:a:b:c:t) -> (getReg a, getBas b, getBas c) >>: lift2 . uncurry3  (executeComp (>)) >> process' t stack
                  (6:x:_) -> (stripNumb <$> (getBas x >>= lift2 . getValue), ask) >>>  (flip process') stack . (uncurry drop)
                  (7:a:b:t) -> (safeInt a, safeInt b, ask) >>: \(adr, i, env) ->  process' (if i /= 0 then drop adr env else t) stack
                  (8:a:b:t) ->  (safeInt a, safeInt b, ask) >>: \(adr, i, env) ->  process' (if i == 0 then drop adr env else t) stack
                  (9:a:b:c:t) -> (getReg a, safeNumb b, safeNumb c) >>: \(reg, add1, add2) -> (lift2 $ executeIn2 reg (B1 $ add1 + add2)) >> process' t stack
                  (22:t) -> liftIO $ putStrLn $ show t
                  --skipped some since they're boring
                  (15:a:b:t) -> (safeInt b, ask) >>> executeRmem t a stack
                  (19:x:t) -> getBas x >>= lift2 . executeOut >> process' t stack
                  (21:t) -> (lift3 . return) executeNoop >> process' t stack
                  where
                  lift2 :: StateT RegisterState IO a -> ExceptT String (ReaderT [Int] (StateT RegisterState IO)) a
                  lift2 = lift . lift
                  lift3 = lift2 . lift
                  safeNumb x = getBas x >>= lift2 . getValue
                  safeInt x = stripNumb <$> (safeNumb x)

getReg :: Monad m => Int -> ExceptT String m (Basic Reg)
getReg = toExcept . getRegister
getNumb :: Monad m => Int -> ExceptT String m (Basic Numb)
getNumb = toExcept . getNumber
getBas :: Monad m => Int -> ExceptT String m (Basica)
getBas = toExcept . getBasic

getValue :: Monad m => Basica -> StateT RegisterState m (Basic Numb)
getValue (B1 b) = return $ b
getValue (B2 (Register r)) = do
                              (RegisterState rs) <- get
                              return $ rs !! r

executeIn1 :: Basic Reg -> Basic Reg -> RegisterState -> RegisterState
executeIn1 r1 (Register r2) s@(RegisterState rs) = modifyRegs r1 (\_ -> rs !! r2) s

executeIn2 :: Monad m => Basic Reg -> Basica -> StateT RegisterState m ()
executeIn2 r1 a = (getValue a, get) >>> (\(n, s) -> put $ modifyRegs r1 (\_ -> n) s)

executePop :: Monad m => Basic Numb -> Basic Reg -> StateT RegisterState m ()
executePop a r = executeIn2 r $ toBasic a

executeComp :: Monad m => (Basic Numb -> Basic Numb -> Bool) -> Basic Reg -> Basica -> Basica -> StateT RegisterState m ()
executeComp f r b1 b2 = (getValue b1, getValue b2) >>> (change . uncurry f)
                                where change pred = executeIn2 r $ B1 $ Number $ if pred then 1 else 0

executeOut :: Basica -> StateT RegisterState IO ()
executeOut (B1 (Number i)) = liftIO . putStrLn . show . chr $ i
executeOut (B2 r) = get >>= liftIO . putStrLn . show . chr . stripNumb . getRegValue r

executeRmem t a stack (val, rs) = let newList = modifyList a rs (\_ _ -> val) in
                           local (\_ -> newList) $ process' ((length rs - length t) `drop` newList) stack

executeNoop :: ()
executeNoop = ()
