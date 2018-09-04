{-# Language QuasiQuotes #-}
{-# Language TemplateHaskell #-}
module TigerQQ where

import TigerAbs
import TigerSymbol as TSym
import TigerParser (parseFromStr)

-- * Quasi-Quotation

import Data.Generics
import qualified Language.Haskell.TH as TH
import Language.Haskell.TH.Syntax as Syn hiding (Exp)
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
      dataToExpQ (const Nothing `extQ` handleSymbol) expr

handleSymbol :: Symbol -> Maybe TH.ExpQ
handleSymbol s = Just $ TH.appE (TH.varE 'TSym.pack) $ TH.litE $ TH.StringL $ unpack s

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

