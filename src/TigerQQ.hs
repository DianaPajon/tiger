module TigerQQ where

import TigerAbs
import TigerParser (parseFromStr)

-- * Quasi-Quotation

import Data.Generics
import qualified Language.Haskell.TH as TH
import qualified Language.Haskell.TH.Syntax as Syn
import Language.Haskell.TH.Quote

-- | Parser de expresiones
-- esto es lo más interesante.
quoteExprExp :: String -> TH.ExpQ
quoteExprExp s =
  do  loc <- TH.location
      let pos =  ( TH.loc_filename loc
                 , fst (TH.loc_start loc)
                 , snd (TH.loc_start loc)
                 )
      expr <- parseFromStr pos s
      Syn.liftData expr
      -- dataToExpQ (const Nothing `extQ` antiExprExpr) expr

-- antiExprExpr :: Exp -> Maybe (TH.Q TH.Exp)
-- antiExprExpr _ = Nothing -- No antipatterns yets

-- | Pattern Matching
quoteExprPat :: String -> TH.PatQ
quoteExprPat s =  do  loc <- TH.location
                      let pos =  (TH.loc_filename loc,
                                 fst (TH.loc_start loc),
                                 snd (TH.loc_start loc))
                      expr <- parseFromStr pos s
                      dataToPatQ (const Nothing `extQ` antiExprPat) expr

antiExprPat :: Exp -> Maybe (TH.Q TH.Pat)
antiExprPat _ = Nothing -- No antipatterns yets

expr :: QuasiQuoter
expr = QuasiQuoter
       {
         quoteExp = quoteExprExp
       , quotePat = quoteExprPat
       , quoteType = undefined
       , quoteDec = undefined -- Mejorar?
       }

