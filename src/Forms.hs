module Forms (Form,
              Binding,
              processSexpr,
              process)
       where

import qualified Parse as P
import Text.PrettyPrint

data Form a = Let { bindings :: [Binding a], body :: [Form a] }
            | FunctionCall { func :: Form a, args :: [Form a]}
            | Symbol String
            | Number Integer
            | Nil
            deriving Eq

data Binding a = Binding { var :: Form a,  val :: Form a } deriving Eq

instance (Show a) => Show (Binding a) where
  show = showBinding

instance (Show a) => Show (Form a) where
  show = showForm

process :: P.Program -> [Form a]
process (P.Program pl) = map processSexpr pl

processSexpr :: P.SExpr -> Form a
processSexpr (P.List (x:xs))
  | x == P.Atom (P.Symbol "let") = Let { bindings = map toBinding (getBindings xs), body = map processSexpr (getBody xs) }
  | otherwise = FunctionCall { func = processSexpr x, args = map processSexpr $ xs }
processSexpr (P.Atom (P.Number n)) = Number n
processSexpr (P.Atom (P.Symbol s)) = Symbol s
processSexpr P.Nil = Nil

toSymbolForm :: P.SExpr -> Form a
toSymbolForm (P.Atom (P.Symbol s)) = Symbol s
toSymbolForm _ = error "Not a Symbol."

toBinding :: P.SExpr -> Binding a
toBinding (P.List [var,gets,val]) = Binding {var = toSymbolForm var, val = processSexpr val }

getFunc :: P.SExpr -> P.SExpr
getFunc (P.List l) = head l
getFunc _ = error "Not a FunctionCall."

getArgs :: P.SExpr -> P.SExpr
getArgs (P.List l) = P.List $ tail l
getArgs _ = error "Not a FunctionCall."

getBindings :: [P.SExpr] -> [P.SExpr]
getBindings xs = filter isBinding xs

getBody :: [P.SExpr] -> [P.SExpr]
getBody xs = filter (not . isBinding) xs  

isBinding :: P.SExpr -> Bool
isBinding (P.List l)
    | (length l) == 3 && (l !! 1) == (P.Atom (P.Symbol "<-")) && isSymbol (l !! 0) = True
    | otherwise = False
isBinding _ = False

isSymbol :: P.SExpr -> Bool
isSymbol (P.Atom (P.Symbol _)) = True
isSymbol _ = False

agProcess :: String -> [Form a]
agProcess s = let parsed = P.agParse s in
  case parsed of
    (Right parsedProgram) -> process parsedProgram
    (Left e) -> []

showForm :: (Show a) => (Form a) -> String
showForm = show . prettyForm

showBinding :: (Show a) => (Binding a) -> String
showBinding = show . prettyBinding

prettyForm :: (Show a) => (Form a) -> Doc
prettyForm Nil = text "nil"
prettyForm (Number n) = integer n
prettyForm (Symbol s) = text s
prettyForm FunctionCall {func = func, args = args} =
  prettyForm func $+$ (vcat $ map (nest 2 . prettyForm) args)
prettyForm Let {bindings = bindings, body = body} =
  text "let" $+$ (vcat $ map (nest 2 . prettyBinding) bindings) $+$ (vcat $ map (nest 2 . prettyForm) body)

prettyBinding :: (Show a) => Binding a -> Doc
prettyBinding Binding { var = var, val = val } =
  parens $ hsep [prettyForm var, text "<-", prettyForm val]

