import qualified Parse as P

data Form a = Let { bindings :: [Binding a], body :: [Form a] }
            | FunctionCall { func :: Form a, args :: [Form a]}
            | Symbol String
            | Number Integer
            | Nil
            deriving (Show, Eq)

data Binding a = Binding { var :: Symbol,  val :: a }

process :: P.Program -> [Form a]
process (P.Program pl) = map processSexpr pl

processSexpr (P.List (x:xs))
  | x == P.Atom (P.Symbol "let") = Let { bindings = (getBindings xs), body = map processSexpr (getBody xs) }
  | otherwise = FunctionCall { func = getFunc x, args = map processSexpr (getArgs x) }
  where
    getBindings xs = filter isBinding xs
    isBinding (P.List l)
    | (length l) == 3 && (l !! 1) == (Atom (Symbol "<-")) = True
processSexpr (P.Atom (P.Number n)) = Number n
processSexpr (P.Atom (P.Symbol s)) = Symbol s
processSexpr (P.Atom P.Nil) = Nil
