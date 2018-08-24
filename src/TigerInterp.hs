{-# Language LambdaCase #-}
{-# Language OverloadedStrings #-}
module TigerInterp where

import Prelude hiding (compare, EQ)

import TigerTree
import TigerFrame
import TigerTemp
import TigerSymbol

import Data.Map as M

import Control.Monad.State

import Debug.Trace

data Dato
    = Str Symbol -- | String constante. Ej: str: "Hola"
    | FBody ([Access], [Stm]) -- | Función, lista de acceso de los argumentos y el body
    | GG Int
    deriving (Show)

getInt :: Dato -> Int
getInt (GG i) = i
getInt _ = error "NOT AN INT"

getStr :: Dato -> Symbol
getStr (Str s) = s
getStr _ = error "NOT A Symbol?"

getFBody :: Dato -> ([Access] , [Stm])
getFBody (FBody sts) = sts
getFBody _ = error "NOT A FUN"

data CPU = CPU
    { mem :: M.Map Temp Int
    , wat :: M.Map Int Dato
    , dat :: M.Map Label Dato
    , output :: [Symbol]
    , input :: [Symbol]
    } deriving Show

type RC = State CPU

extCall :: Label -> Bool
extCall l = all ((== l) . pack) ["print", "flush", "getchar"]

printExec :: Int -> RC Int
printExec i = trace (show i) $ do
  env <- get
  let mblab = wat env ! i
  let mbstr = dat env ! getStr mblab
  put (env{output = output env ++ [getStr mbstr]} )
  return 1

extDispatcher :: Label -> [Int] -> RC Int
extDispatcher "print" (x : _ ) = printExec x

compute :: BOp -> Int -> Int -> Int
compute Plus = (+)
compute _ = error "TODO"

compare :: Relop -> Int -> Int -> Bool
compare EQ = (==)
compare _ = error "TODO"

-- | Exp :: TInt
iexp :: Exp -> RC Int
iexp (Const i) = return i
iexp (Name n) = trace ("NAME") $ get >>= \e -> return $ getInt (dat e ! n)
iexp (Temp t) = get >>= \e -> return $ mem e ! t
iexp (Binop op x y) = do
  x' <- iexp x
  y' <- iexp y
  return $ compute op x' y'
iexp (Mem e) = do
  e' <- iexp e
  env <- get
  return $ getInt $ wat env ! e'
iexp (Call (Name f) es) = do
  es' <- mapM iexp es
  dats <- gets dat
  if (extCall f)
     then extDispatcher f es'
  -- Acá hay que conectar los argumentos con el body de la función
     else do
        let (acc , body) = getFBody $ dats ! f
        mapM_ step $ zipWith (\a i -> Move (TigerFrame.exp a 0) (Mem (Const i))) acc es'
        mems <- gets mem
        return $ mems ! rv
iexp (Call _ _ ) = error "Puede pasar?"
iexp (Eseq s e) = step s >> iexp e

step :: Stm -> RC [Stm]
step (Label _) = return []
step (Seq l r) = return [l , r]
-- | Assm load
step (Move (Temp t) (Mem m)) = do
  dir <- iexp m
  wats <- gets wat
  modify $ \env -> env{mem = M.insert t (getInt $ wats ! dir) (mem env)}
  return []
step (Move (Temp t) src) = do
  val <- iexp src
  modify $ \env -> env{mem = M.insert t val (mem env)}
  return []
-- | Assm store
step (Move (Mem t) src) = do
  dir <- iexp t
  val <- iexp src
  modify $ \env -> env{wat = M.insert dir (GG val) (wat env)}
  return []
step (Move dst src) = do
  src' <- iexp src
  dst' <- iexp dst
  modify (\env -> env{wat = M.insert dst' (wat env ! src') (wat env)})
  return []
step (ExpS e) = iexp e >> return []
step (Jump _ l) = gets dat >>= \dats -> return $ snd $ getFBody $ dats ! l
step (CJump bop x y tt ff) = do
  x' <- iexp x
  y' <- iexp y
  return $ if compare bop x' y' then
    [Jump (Const 0) tt]
  else
    [Jump (Const 0) ff]

runPc :: [Stm] -> RC ()
runPc [] = return ()
runPc (l@Jump{} : _) = step l >>= runPc
runPc (x : xs) = step x >>= \ys -> runPc (ys ++ xs)

emptyCPU :: CPU
emptyCPU = CPU M.empty M.empty M.empty [] []

runInitial :: CPU -> [Stm] -> CPU
runInitial cpu prog = execState (runPc prog) cpu

loadCPU :: [(Frame, [Stm])] -- | Datos
        -> [(Label , Symbol)]
        -> [Stm]  -- | TigerMain
        -> CPU
loadCPU fs ss = runInitial
                (Prelude.foldl (\r (f, body) ->
                                  r{dat = M.insert (name f)
                                           (FBody ( prepFormals f , body))
                                           (dat r)
                                    }
                               )
                (Prelude.foldl (\r (l, s) ->
                                  r{dat = M.insert l (Str s) (dat r)}
                                  ) emptyCPU ss) fs )

