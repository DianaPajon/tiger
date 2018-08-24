module TigerTree where

import qualified TigerTemp as Temp

import           Prelude   hiding (EQ, GT, LT)

data Exp
    = Const Int
    | Name Temp.Label
    | Temp Temp.Temp
    | Binop BOp Exp Exp
    | Mem Exp
    | Call Exp [Exp]
    | Eseq Stm Exp
    deriving Show

data Stm
    = Move Exp Exp
    | ExpS Exp
    | Jump Exp Temp.Label
    | CJump Relop Exp Exp Temp.Label Temp.Label
    | Seq Stm Stm
    | Label Temp.Label
    deriving Show

data BOp = Plus | Minus | Mul | Div | And | Or | LShift | RShift
    | ARShift | XOr
    deriving Show

data Relop = EQ | NE | LT | GT | LE | GE | ULT | ULE | UGT | UGE
    deriving Show

notRel :: Relop -> Relop
notRel EQ  = NE
notRel NE  = EQ
notRel LT  = GE
notRel GE  = LT
notRel GT  = LE
notRel LE  = GT
notRel ULT=UGE
notRel UGE=ULT
notRel ULE = UGT
notRel UGT = ULE
