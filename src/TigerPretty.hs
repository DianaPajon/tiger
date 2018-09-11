module TigerPretty where

import           Prelude          hiding ((<>))
import           TigerAbs

import           Text.PrettyPrint
import           TigerSymbol

tabWidth :: Int
tabWidth = 8

prettyVar :: Var -> Doc
prettyVar (SimpleVar s)      = text $ unpack s
prettyVar (FieldVar v s)     = prettyVar v <> text "."  <> (text $ unpack s)
prettyVar (SubscriptVar v e) = prettyVar v <> (brackets $ prettyExp e)

prettyOp :: Oper -> Doc
prettyOp PlusOp   = text "+"
prettyOp MinusOp  = text "-"
prettyOp TimesOp  = text "*"
prettyOp DivideOp = text "/"
prettyOp EqOp     = text "="
prettyOp NeqOp    = text "<>"
prettyOp LtOp     = text "<"
prettyOp GtOp     = text ">"
prettyOp LeOp     = text "<="
prettyOp GeOp     = text ">="

-- | Completar si quieren mejorar el pp
prettyTy :: Ty -> Doc
prettyTy = text . show

-- | Completar si quieren mejorar el pp
prettyField :: [(Symbol, Escapa , Ty)] -> Doc
prettyField = text . show

prettyDec  :: Dec -> Doc
prettyDec (FunctionDec f) = vcat $ map functionDec f
  where
    functionDec (s, f, Just r, e, _) =
      hang (text "function " <> (text $ unpack s) <> (parens $ prettyField f) <> text " : " <> (text $ unpack r) <> (text " = ")) tabWidth (prettyExp e)
    functionDec (s, f, Nothing, e, _) = hang (text "function " <> (text $ unpack s) <> (parens $ prettyField f) <> (text " = ")) tabWidth (prettyExp e)

prettyDec (VarDec s _ (Just r) e _) = (text $ unpack s) <> text " : " <> (text $ unpack r) <> text " = " <> prettyExp e
prettyDec (VarDec s _ Nothing e _) = (text $ unpack s) <> text " = " <> prettyExp e
prettyDec (TypeDec f) = vcat $ map typeDec f
  where
    typeDec (s, ty, _) = text "type " <> (text $ unpack s) <> text " = " <> prettyTy ty

prettyExp :: Exp -> Doc
prettyExp (VarExp v _) = prettyVar v
prettyExp (UnitExp _) = text "()"
prettyExp (NilExp _) = text "nil"
prettyExp (IntExp i _) = text $ show i
prettyExp (StringExp s _) = doubleQuotes $ text s
prettyExp (CallExp s args _) = text (unpack s) <> (parens $ hcat $ punctuate comma $ map prettyExp args)
prettyExp (OpExp e1 op e2 _) = prettyExp e1 <> prettyOp op <> prettyExp e2
prettyExp (RecordExp r n _) = error "COMPLETAR PRINTER"
prettyExp (SeqExp e _) = parens $ vcat $ punctuate semi (map prettyExp e)
prettyExp (AssignExp v e _) = prettyVar v <> text " = " <> prettyExp e
prettyExp (IfExp e e1 (Just e2) _) = (hang (text "if " <> prettyExp e <> text " then ") tabWidth (prettyExp e1)) $$ text "else " <> prettyExp e2
prettyExp (IfExp e e1 Nothing _) = hang (text "if " <> prettyExp e <> text " then ") tabWidth (prettyExp e1)
prettyExp (WhileExp e e1 _) = hang (text "while " <> prettyExp e) tabWidth (prettyExp e1)
prettyExp (ForExp s _ e1 e2 e3 _) = hang (text "for " <> text (unpack s) <> text " := " <> prettyExp e1 <> text " to " <> prettyExp e2) tabWidth (prettyExp e3)
prettyExp (LetExp d e _) = text "let " <> nest tabWidth (vcat (map prettyDec d)) $$ text "in " <> prettyExp e $$ text "end"
prettyExp (BreakExp _) = text "break"
prettyExp (ArrayExp s e1 e2 _) = text "array " <> brackets (prettyExp e1) <> text " of " <> prettyExp e2

renderExp :: Exp -> String
renderExp = render . prettyExp
