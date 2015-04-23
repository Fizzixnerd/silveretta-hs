module Forms (Form(Let, FunctionCall, Symbol, Number, Function, Nil),
              bindings, body, func, args,
              Binding(Binding),
              var, val,
              BindingVar,
              processSexpr,
              process,
              agProcess,
              fmain)
       where

import qualified Parse as P
import Text.PrettyPrint

data Form = Let { bindings :: [Binding], body :: Form }
          | If {predic :: Form, conseq :: Form, ante :: Form}
          | FunctionCall { func :: Form, args :: [Form]}
          | Symbol String
          | Number Double
          | Function (Form -> Form)
          | Nil

instance Eq Form where
  Let {bindings = bi, body = bo} == Let {bindings = bi2, body = bo2} = (bi == bi2) && (bo == bo2)
  FunctionCall {func=f, args=a} == FunctionCall {func=f2, args=a2} = (f == f2) && (a == a2)
  Symbol s == Symbol s2 = s == s2
  Number n == Number n2 = n == n2
  Nil == Nil = True
  If {predic = p, conseq = c, ante = a} == If {predic = p2, conseq = c2, ante = a2} =
    (p == p2) && (c == c2) && (a == a2)
  _ == _ = False

data Binding = Binding { var :: BindingVar,  val :: Form } deriving Eq

type BindingVar = String

instance Show Binding where
  show = showBinding

instance Show Form where
  show = showForm

instance Ord Form where
  compare (Symbol x) (Symbol y) = compare x y
  compare (Number x) (Number y) = compare x y

process :: P.Program -> [Form]
process (P.Program pl) = map processSexpr pl

processSexpr :: P.SExpr -> Form
processSexpr (P.List (x:xs))
  | x == P.Atom (P.Symbol "let") = Let { bindings = map toBinding (getLetBindings xs),
                                         body = processSexpr (getLetBody xs) }
  | x == P.Atom (P.Symbol "if") = If { predic = processSexpr (getPredicate xs),
                                       conseq = processSexpr (getIfConsequent xs),
                                       ante =  processSexpr (getIfAntecedent xs) }
  | otherwise = FunctionCall { func = processSexpr x , args = map processSexpr xs }
processSexpr (P.Atom (P.Number n)) = Number n
processSexpr (P.Atom (P.Symbol s)) = Symbol s
processSexpr P.Nil = Nil

toBindingVar :: P.SExpr -> String
toBindingVar (P.Atom (P.Symbol s)) = s
toBindingVar _ = error "Not a Symbol."

toBinding :: P.SExpr -> Binding
toBinding (P.List [var,gets,val]) = Binding {var = toBindingVar var, val = processSexpr val }

getFunc :: P.SExpr -> P.SExpr
getFunc (P.List l) = head l
getFunc _ = error "Not a FunctionCall."

getArgs :: P.SExpr -> P.SExpr
getArgs (P.List l) = P.List $ tail l
getArgs _ = error "Not a FunctionCall."

getLetBindings :: [P.SExpr] -> [P.SExpr]
getLetBindings xs = filter isBinding xs

getLetBody :: [P.SExpr] -> P.SExpr
getLetBody xs =
  let body = filter (not . isBinding) xs in
  if (length body) > 1 then
    error "`Let' body has more than one Form."
  else
    head body

getIfBody :: [P.SExpr] -> P

isBinding :: P.SExpr -> Bool
isBinding (P.List l)
    | (length l) == 3 && (l !! 1) == (P.Atom (P.Symbol "<-")) && isSymbol (l !! 0) = True
    | otherwise = False
isBinding _ = False

isSymbol :: P.SExpr -> Bool
isSymbol (P.Atom (P.Symbol _)) = True
isSymbol _ = False

agProcess s = fmap process $ P.agParse s
{-
agProcess s = P.agParse s in
  case parsed of
    (Right parsedProgram) -> process parsedProgram
    (Left e) -> []
-}

showForm :: Form -> String
showForm = show . prettyForm

showBinding :: Binding -> String
showBinding = show . prettyBinding

prettyForm :: Form -> Doc
prettyForm Nil = text "nil"
prettyForm (Number n) = double n
prettyForm (Symbol s) = text s
prettyForm (Function f) = text "#<function>"
prettyForm FunctionCall {func = func, args = args} =
  prettyForm func
  $+$ (vcat $ map (nest 2 . prettyForm) args)
prettyForm Let {bindings = bindings, body = body} =
  text "let"
  $+$ (vcat $ map (nest 2 . prettyBinding) bindings)
  $+$ ((nest 2 . prettyForm) $ body)
prettyForm If {predic = predic, conseq = conseq, anti = anti} =
  text "if ("
  $+$ (prettyForm predic)
  $+$ (text ")")
  $+$ (vcat $ map (nest 2 . prettyForm) [conseq, anti])

prettyBinding :: Binding -> Doc
prettyBinding Binding { var = var, val = val } =
  hsep [text $ show var, text "<-", prettyForm val]

fmain = agProcess P.testText5
