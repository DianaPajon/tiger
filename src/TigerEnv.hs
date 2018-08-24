module TigerEnv where

-- import qualified Data.Map.Strict as M
-- import qualified Data.List as L

class Environmental w where
    lookupI ::(Ord a) => a -> w a d -> Maybe d -- Buscar
    insertI ::(Ord a) => a -> d -> w a d -> w a d
    intersecI :: (Ord a) =>  (d -> d -> d) -> w a d -> w a d -> w a d
    updateI :: (Ord a) => a -> d -> w a d -> w a d
    emptyI :: w a d

fromList :: (Ord a, Environmental m) => [(a,k)] -> m a k
fromList = foldl (\env (k,d) -> insertI k d env) emptyI
